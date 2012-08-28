
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
