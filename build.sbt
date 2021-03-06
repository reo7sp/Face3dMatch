name := "Face3dMatch"

// compile options
javacOptions ++= Seq("-source", "1.7", "-target", "1.7")
scalaVersion := "2.11.7"
scalacOptions in Compile ++= Seq("-optimise")

// android
import android.Keys._
minSdkVersion in Android := "16"
targetSdkVersion in Android := "23"
platformTarget in Android := "android-23"
dexMulti in Android := true
proguardOptions in Android ++= Seq("-dontobfuscate", "-dontwarn scala.collection.**", "-dontwarn org.scaloid.**")

// dependencies
resolvers += Resolver.jcenterRepo
libraryDependencies ++= Seq(
  aar("com.android.support" % "multidex" % "1.0.1"),
  "javax.inject" % "javax.inject" % "1",
  "org.scaloid" %% "scaloid" % "4.1",
  "org.json4s" %% "json4s-native" % "3.3.0"
//  "org.openpnp" % "opencv" % "2.4.11-2"
)

// tasks
run <<= run in Android
install <<= install in Android
