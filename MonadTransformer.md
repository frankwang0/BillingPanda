WriterT is an instance of the MonadIO class, see here:

http://hackage.haskell.org/packages/archive/transformers/0.2.2.0/doc/html/Control-Monad-IO-Class.html

This means that there is an implementation of liftIO (the only method defined for the MonadIO class) which has the type:

liftIO :: IO a -> WriterT w m a

Oh, and an even more important fact is that WriterT w IO a is a monad, and therefore has the bind operator (>>=) defined for it. This bind operator apparently "threads" the monadic payload of *both* the Writer monad and the IO monad, in between the actions in the "do" block.

That's why we're able to call an IO action inside the do block - the "monadic environment" we run in includes both IO and Writer.

But we can normally access only the "Writer" monad, not the IO one. That is, the <- operator takes out values from Writer, "return" returns values into Writer. This is probably because the do notation relies on "bind" (>>=) and "return", and they both support only a Monad with a single type paramter.

So in order to access the IO monad, liftIO is defined for WriterT, and its implementation takes the monadic payload from the IO moand, and puts it into the "Writer" part of the WriterT monad, so we can access it in normal do notation.

Oh, and liftIO isn't just for working for monad transformers. It was explained back in chapter 16, where it was used with a type which "wrapped" the IO monad and exposed only well-defined subset of its actions. There, liftIO was explicitly implemented as a way to access into the "inner" IO monad.

Finally, execWriter, presented below in the ghci snippets, is both a way to supply values to the WriterT instance, but it also supplies it with its IO monad in which it can run (this isn't immediately obvious from its type signature unless you remember that a function which returns IO, or any other monad, actually accepts one because under the covers it returns its first bind operator - I analyzed this at length back in chapter 10).

All of this wasn't exactly trivial to understand. but it took me less than a hour, which is a vast improvement over the previous chapter. Maybe I'm finally getting the hang of this.

ghci> :type runWriterT
runWriterT :: WriterT w m a -> m (a, w)

ghci> :type execWriterT
execWriterT :: (Monad m) => WriterT w m a -> m w

ghci> :type countEntries ".."
countEntries ".." :: WriterT [(FilePath, Int)] IO ()

ghci> :type execWriterT (countEntries "..")
execWriterT (countEntries "..") :: IO [(FilePath, Int)]

ghci> take 4 `liftM` execWriterT (countEntries "..")
[("..",30),("../ch15",23),("../ch07",26),("../ch01",3)]