import Control.Monad.ST
import Data.Array.ST
import Data.STRef

data Block = Free | Allocated
type Pool s = STArray s Int Block
type FreeList s = STRef s [Int]

data PoolAllocator s = PoolAllocator {
    blockSize :: Int,
    numBlocks :: Int,
    freeList :: FreeList s,
    pool :: Pool s
}

createPool :: Int -> Int -> ST s (PoolAllocator s)
createPool blockSize numBlocks = do
    pool <- newArray (0, numBlocks) Free
    freeList <- newSTRef [0..numBlocks - 1]
    return $ PoolAllocator {
        blockSize = blockSize,
        numBlocks = numBlocks,
        freeList = freeList,
        pool = pool
    }

allocate :: PoolAllocator s -> ST s (Maybe Int)
allocate alloc = do
    free <- readSTRef (freeList alloc)
    case free of
        [] -> return Nothing
        (idx : rest) -> do
            writeArray (pool alloc) idx Allocated
            writeSTRef (freeList alloc) rest
            return (Just idx)

deallocate :: PoolAllocator s -> Int -> ST s ()
deallocate dealloc idx = do
    writeArray (pool dealloc) idx Free
    modifySTRef' (freeList dealloc) (idx :)
