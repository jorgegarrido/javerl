
.PHONY: compile clean

compile:
	for FILE in `ls src/ | grep .erl`;\
                do\
                erlc -I include/ -o ebin/ src/$$FILE;\
                echo "erlc -I include/ -o ebin/ $$FILE";\
        done

clean:
	for FILE in `ls ebin/ | grep .beam`;\
                do\
                rm -rf ebin/$$FILE;\
                echo "rm -rf ebin/$$FILE";\
        done
	rm -rf java_src/Javerl.class

example:
	(cd java_src && javac Javerl.java && erl -pa ../ebin/ -eval 'ok = application:start(javerl)')
