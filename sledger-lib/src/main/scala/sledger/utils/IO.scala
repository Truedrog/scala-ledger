package sledger.utils
import cats.effect.{Resource, Sync}

import java.io._
object IO {
  def readFileOrStdin[F[_]: Sync](filePath: String): F[String] = {
    val fileResource: Resource[F, InputStream] =
      if (filePath == "-") {
        Resource.eval(Sync[F].blocking(System.in))
      } else {
        Resource.fromAutoCloseable(Sync[F].blocking(new FileInputStream(filePath)))
      }

    fileResource.use { inputStream =>
      val reader = new BufferedReader(new InputStreamReader(inputStream))
      Sync[F].delay {
        val stringBuilder = new StringBuilder()
        var line: String = null
        while ({ line = reader.readLine(); line != null }) {
          stringBuilder.append(line).append("\n")
        }
        stringBuilder.toString()
      }
    }
  }

  def writeFile[F[_]: Sync](fileName: String, content: String): F[Unit] =
    Resource
      .fromAutoCloseable(Sync[F].delay(new BufferedWriter(new FileWriter(new File(fileName)))))
      .use { writer =>
        Sync[F].delay(writer.write(content))
      }
}
