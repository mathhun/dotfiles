load.ivy("org.scalaz" %% "scalaz-core" % "7.2.4")
load.ivy("com.chuusai" %% "shapeless" % "2.3.1")

/*********************************************************************************
//
// pretty print
//
@ show(Seq.fill(30)(100), height=3)
@ pprintConfig() = pprintConfig().copy(height = 5)

//
// import
//
@ import $file.MyScript._
@ import $exec.MyScript._
@ import $ivy.`org.scalaz::scalaz-core:7.2.4`, scalaz._, Scalaz._

//
// desugar
//
@ import scalaz.Reader, scalaz.syntax.applicative._
@ type ReaderInt[A] = Reader[Int, A]
@ desugar { "foo".point[ReaderInt] }

//
// save/load session
//
@ sess.save()
@ sess.load()
@ sess.save("xy initialized")
@ sess.load("xy initialized")

*********************************************************************************/
