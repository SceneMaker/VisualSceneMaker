#!/usr/bin/env sh
echo "Generating ScriptLexxer.java"
java -cp lib/JFlex.jar JFlex.Main core/src/main/java/de/dfki/vsm/model/scenescript/lexxer.jflex
echo "Generating ScriptFields.java and ScriptParser.java"
java -cp lib/JCup.jar java_cup.Main -progress -time -interface -parser ScriptParser -symbols ScriptFields -destdir core/src/main/java/de/dfki/vsm/model/scenescript/ core/src/main/java/de/dfki/vsm/model/scenescript/parser.jcup

echo "All done."
