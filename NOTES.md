# Running NetLogo

open a bash shell and run:

~~~
> cd /c/Program\ Files\ \(x86\)/NetLogo\ 5.0.3/
> time java -Xmx1024m -Dfile.encoding=UTF-8 -cp NetLogo.jar org.nlogo.headless.Main --model "C:\Users\Nikolaos\Dropbox\Repos\hlogo\bench\NetLogo\RedBlue1000\RedBlue1000.nlogo" --experiment experiment1
~~~

# Running HLogo

open a bash shell and run:

~~~
> cd /c/Users/Nikolaos/Dropbox/Repos/hlogo/
> time ./dist/build/redblue1000/redblue1000 --max-pxcor=100 --min-pxcor=-100 --max-pycor=100 --min-pycor=-100 -h -v +RTS -N4 -H1G
~~~

in the RTS one of the following options increases performance: `-A1G` to enlarge GC space or `-H1G` to enlarge heap space
