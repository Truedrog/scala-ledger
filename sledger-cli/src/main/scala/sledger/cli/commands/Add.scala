package sledger.cli.commands

object Add {
  //  
  //  def runConsoleLoop(terminal: Terminal, lineRef: Ref[IO, String]): IO[Unit] = {
  //    val reader = LineReaderBuilder.builder().terminal(terminal).build()
  //
  //    def loop: IO[Unit] = {
  //      for {
  //        line <- IO.delay(reader.readLine("Enter your input: "))
  //        _ <- lineRef.set(line)
  //        _ <- line match {
  //          case "exit" => IO.unit
  //          case _ => loop
  //        }
  //      } yield ()
  //    }
  //
  //    loop
  //  }
  //  
  //  val add = for {
  //    terminal <- IO(TerminalBuilder.builder().dumb(false).system(true).build())
  //    lineRef <- Ref.of[IO, String]("")
  //    _ <- runConsoleLoop(terminal, lineRef)
  //  } yield ExitCode.Success
}
