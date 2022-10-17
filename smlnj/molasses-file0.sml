(* parse-sml/src/base/lib/compat/nj-mlton.sml : 1.1-428.1 *)
(* molasses-file0.sml *)
(* Mostly taken from the following places:
 * https://github.com/MPLLang/mpl/blob/master/lib/stubs/common/mlton/process-via-fork-exec.sml
 * https://github.com/MLton/mlton/blob/master/basis-library/mlton/process.sml
 * with some modifications
 *)

(* Copyright (C) 2013,2019,2022 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
structure MLtonProcess =
  struct
    type pid = Posix.Process.pid

    local
      fun mk (exec, args) =
        case Posix.Process.fork () of
          NONE => exec args
        | SOME pid => pid
    in
      fun spawne {path, args, env} = mk (Posix.Process.exece, (path, args, env))
      fun spawnp {file, args} = mk (Posix.Process.execp, (file, args))
    end

    fun spawn {path, args} =
      spawne {path = path, args = args, env = Posix.ProcEnv.environ ()}
  end

(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2002-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
structure MLton =
  struct
    val isMLton = false

    fun eq (a : 'a, b : 'a) = (Unsafe.cast a : word) = Unsafe.cast b
    (* fun equal (a : 'a, b : 'a) = (Unsafe.cast a : ''b) = Unsafe.cast b *)

    structure Exn =
      struct
        val history = SMLofNJ.exnHistory
      end

    structure GC =
      struct
        fun collect () = SMLofNJ.Internals.GC.doGC 8
        fun pack () = collect ()
      end

    structure Process =
      struct
        open MLtonProcess

        structure OldIO = IO
        local
          open Posix
        in
          structure FileSys = FileSys
          structure IO = IO
          structure ProcEnv = ProcEnv
          structure Process = Process
          (* structure FileDesc = PrePosix.FileDesc
          structure PId = PrePosix.PId *)
          structure Signal = Signal
        end

        exception MisuseOfForget
        exception DoublyRedirected

        type input = unit
        type output = unit

        type none = unit
        type chain = unit
        type any = unit

        (* TODO fix *)
        val useWindowsProcess = false
        (* MLton.Platform.OS.useWindowsProcess *)

        val readWrite =
          let open FileSys.S in
            flags [irusr, iwusr, irgrp, iwgrp, iroth, iwoth]
          end

        structure Child =
          struct
            datatype 'use childt =
            FileDesc of IO.file_desc | Stream of 'use * ('use -> unit) | Term
            type ('use, 'dir) t = 'use childt ref

            (* This is _not_ the identity; by rebuilding it we get type
              * ('a, 'b) t -> ('c, 'd) t
              *)
            fun remember x =
              case ! x of
                FileDesc f => (x := Stream ((), fn () => ()); ref (FileDesc f))
              | Stream _ => raise MisuseOfForget
              (* remember twice = bad *)
              | Term => ref Term

            local
              fun convert (new, close) p =
                case ! p of
                  FileDesc fd =>
                    let
                      val str = new (fd, "<process>")
                      val () = p := Stream (str, close)
                    in
                      str
                    end
                | Stream (str, _) => str
                | Term => raise MisuseOfForget

              val buf_size = 4096

              fun binio_newIn (fd, name) =
                let
                  val reader =
                    Posix.IO.mkBinReader
                      {fd = fd, initBlkMode = true, name = name}
                in
                  BinIO.mkInstream
                    (BinIO.StreamIO.mkInstream
                       (reader, IO.readVec (fd, buf_size)))
                end
              fun textio_newIn (fd, name) =
                let
                  val reader =
                    Posix.IO.mkTextReader
                      {fd = fd, initBlkMode = true, name = name}
                in
                  TextIO.mkInstream (TextIO.StreamIO.mkInstream (reader, ""))
                end
              fun newOut mk mkSIO (fd, name) =
                let
                  val writer =
                    mk
                      { appendMode = false
                      , chunkSize = buf_size
                      , fd = fd
                      , initBlkMode = true
                      , name = name
                      }
                  val buffer_mode =
                    if Posix.ProcEnv.isatty fd then
                      OldIO.LINE_BUF
                    else
                      OldIO.BLOCK_BUF
                in
                  mkSIO (writer, buffer_mode)
                end
            in
              val binIn : (BinIO.instream, input) t -> BinIO.instream =
                (convert
                   (binio_newIn, BinIO.StreamIO.closeIn o BinIO.getInstream))
              val binOut : (BinIO.outstream, input) t -> BinIO.outstream =
                convert
                  ( newOut Posix.IO.mkBinWriter
                      (BinIO.mkOutstream o BinIO.StreamIO.mkOutstream)
                  , BinIO.closeOut
                  )
              val textIn : (TextIO.instream, input) t -> TextIO.instream =
                convert
                  (textio_newIn, TextIO.StreamIO.closeIn o TextIO.getInstream)
              val textOut : (TextIO.outstream, input) t -> TextIO.outstream =
                convert
                  ( newOut Posix.IO.mkTextWriter
                      (TextIO.mkOutstream o TextIO.StreamIO.mkOutstream)
                  , TextIO.StreamIO.closeOut o TextIO.getOutstream
                  )
            end

            fun fd p =
              case ! p of
                FileDesc fd => fd
              | _ => raise MisuseOfForget

            fun close ch =
              case ch of
                FileDesc fd => IO.close fd
              | Stream (str, close) => close str
              | Term => ()

            val close =
              fn (stdin, stdout, stderr) =>
                (close stdin; close stdout; close stderr)
          end

        structure Param =
          struct
            datatype ('use, 'dir) t =
            File of string | FileDesc of FileSys.file_desc | Pipe | Self

            (* This is _not_ the identity; by rebuilding it we get type
              * ('a, 'b) t -> ('c, 'd) t
              *)
            val forget =
              fn File x => File x
               | FileDesc f => FileDesc f
               | Pipe => Pipe
               | Self => Self

            val pipe = Pipe
            local
              val null = if useWindowsProcess then "nul" else "/dev/null"
            in
              val null = File null
            end
            val self = Self
            fun file f = File f
            fun fd f = FileDesc f

            fun child c =
              FileDesc
                (case ! c of
                   Child.FileDesc f => (c := Child.Stream ((), fn () => ()); f)
                 | Child.Stream _ => raise DoublyRedirected
                 | Child.Term => raise MisuseOfForget)

            fun setCloseExec fd =
              if useWindowsProcess then
                ()
              else
                IO.setfd (fd, IO.FD.flags [IO.FD.cloexec])

            local
              fun openOut std p =
                case p of
                  File s => (FileSys.creat (s, readWrite), Child.Term)
                | FileDesc f => (f, Child.Term)
                | Pipe =>
                    let
                      val {infd, outfd} = IO.pipe ()
                      val () = setCloseExec infd
                    in
                      (outfd, Child.FileDesc infd)
                    end
                | Self => (std, Child.Term)
            in
              fun openStdout p = openOut FileSys.stdout p
              fun openStderr p = openOut FileSys.stderr p
            end

            fun openStdin p =
              case p of
                File s =>
                  ( FileSys.openf (s, FileSys.O_RDONLY, FileSys.O.flags [])
                  , Child.Term
                  )
              | FileDesc f => (f, Child.Term)
              | Pipe =>
                  let
                    val {infd, outfd} = IO.pipe ()
                    val () = setCloseExec outfd
                  in
                    (infd, Child.FileDesc outfd)
                  end
              | Self => (FileSys.stdin, Child.Term)

            fun close p fd =
              case p of
                File _ => IO.close fd
              | FileDesc _ => IO.close fd
              | Pipe => IO.close fd
              | _ => ()
          end

        datatype ('stdin, 'stdout, 'stderr) t =
          T of
              { pid : Process.pid
              , (* if useWindowsProcess,
                 * then this is a Windows process handle
                 * and can't be passed to
                 * Posix.Process.* functions.
                 *) status : Posix.Process.exit_status option ref
              , stderr : ('stderr, input) Child.t
              , stdin : ('stdin, output) Child.t
              , stdout : ('stdout, input) Child.t
              }

        local
          fun make f (T r) = f r
        in
          val getStderr = fn z => make # stderr z
          val getStdin = fn z => make # stdin z
          val getStdout = fn z => make # stdout z
        end

        (* TODO: implement *)
        fun ('a, 'b) protect (f : 'a -> 'b, x : 'a) : 'b = f x
        (* let
          val () = Mask.block Mask.all
        in
          DynamicWind.wind (fn () => f x, fn () => Mask.unblock Mask.all)
        end *)

        local
          fun reap reapFn (T {pid, status, stderr, stdin, stdout, ...}) =
            case ! status of
              NONE =>
                let
                  val _ = Child.close (! stdin, ! stdout, ! stderr)
                  val st = reapFn pid
                in
                  status := SOME st; st
                end
            | SOME st => st
        in
          fun reapForFork p =
            reap
              (fn pid =>
                 let
                   (* protect is probably too much; typically, one
                   * would only mask SIGINT, SIGQUIT and SIGHUP.
                   *)
                   val (_, st) =
                     protect (Process.waitpid, (Process.W_CHILD pid, []))
                 in
                   st
                 end)
              p
          fun reapForCreate p = raise Fail "TODO Windows!"
        (* reap (fn pid =>
              let
                  val pid' = PId.toRep pid
                  val status' = ref (C_Status.fromInt 0)
                  val () =
                    SysCall.simple
                    (fn () =>
                      PrimitiveFFI.Windows.Process.getexitcode
                      (pid', status'))
              in
                  Process.fromStatus' (!status')
              end)
              p *)
        end
        val reap =
          fn p => (if useWindowsProcess then reapForCreate else reapForFork) p

        local
          fun kill killFn (p as T {pid, status, ...}, signal) =
            case ! status of
              NONE => let val () = killFn (pid, signal) in ignore (reap p) end
            | SOME _ => ()
        in
          fun killForFork p =
            kill (fn (pid, signal) => Process.kill (Process.K_PROC pid, signal))
              p
          fun killForCreate p = raise Fail "TODO Windows!"
        (* kill (fn (pid, signal) =>
              SysCall.simple
              (fn () =>
                PrimitiveFFI.Windows.Process.terminate
                (PId.toRep pid, Signal.toRep signal)))
              p *)
        end
        val kill =
          fn (p, signal) =>
            (if useWindowsProcess then
               killForCreate
             else
               killForFork)
              (p, signal)

        fun launchWithFork (path, args, env, stdin, stdout, stderr) =
          case protect (Process.fork, ()) of
            NONE =>
              (* child *)
              let
                fun dup2 (old, new) =
                  if old = new then
                    ()
                  else
                    (IO.dup2 {old = old, new = new}; IO.close old)
                val args = path :: args
                val execTh =
                  case env of
                    NONE => (fn () => Process.exec (path, args))
                  | SOME env => (fn () => Process.exece (path, args, env))
              in
                dup2 (stdin, FileSys.stdin);
                dup2 (stdout, FileSys.stdout);
                dup2 (stderr, FileSys.stderr);
                ignore (execTh ());
                Process.exit 0w127
              (* just in case *)
              end
          | SOME pid => pid
        (* parent *)

        (* TODO windows *)
        val launch = fn z => launchWithFork z
        (* (if useWindowsProcess then launchWithCreate else launchWithFork) z *)

        fun create {args, env, path, stderr, stdin, stdout} =
          if not (FileSys.access (path, [FileSys.A_EXEC])) then
            raise
              Fail
                ("PosixError: "
                 ^ Posix.Error.errorName Posix.Error.noent
                 ^ " -- "
                 ^ Posix.Error.errorMsg Posix.Error.noent
                 ^ "\n")
          else
            let
              val () = TextIO.flushOut TextIO.stdOut
              val (fstdin, cstdin) = Param.openStdin stdin
              val (fstdout, cstdout) = Param.openStdout stdout
              val (fstderr, cstderr) = Param.openStderr stderr
              val closeStdio =
                fn () =>
                  ( Param.close stdin fstdin
                  ; Param.close stdout fstdout
                  ; Param.close stderr fstderr
                  )
              val pid =
                launch (path, args, env, fstdin, fstdout, fstderr)
                  handle
                      ex =>
                        ( closeStdio ()
                        ; Child.close (cstdin, cstdout, cstderr)
                        ; raise ex
                        )
              val () = closeStdio ()
            in
              T
                { pid = pid
                , status = ref NONE
                , stderr = ref cstderr
                , stdin = ref cstdin
                , stdout = ref cstdout
                }
            end
      end
  end

