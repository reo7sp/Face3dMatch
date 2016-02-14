name := "Face3dMatch"

// compile options
javacOptions ++= Seq("-source", "1.7", "-target", "1.7")
scalaVersion := "2.11.7"
scalacOptions in Compile += "-feature"

// android
import android.Keys._
minSdkVersion in Android := "16"
targetSdkVersion in Android := "23"
platformTarget in Android := "android-23"
dexMulti in Android := true
updateCheck in Android := {}
proguardCache in Android ++= Seq("org.scaloid")
proguardOptions in Android ++= Seq("-dontobfuscate", "-dontoptimize", "-keepattributes Signature", "-printseeds target/seeds.txt", "-printusage target/usage.txt"
  , "-dontwarn scala.collection.**"
  , "-dontwarn org.scaloid.**"
)

// dependencies
libraryDependencies ++= Seq(
  aar("com.android.support" % "multidex" % "1.0.1"),
  "org.scaloid" %% "scaloid" % "4.1"
)

// tasks
run <<= run in Android
install <<= install in Android
