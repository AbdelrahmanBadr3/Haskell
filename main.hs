data Item =I [Char]  deriving (Show,Eq)
data User =U [Char]  deriving (Show,Eq)
data Rating c =NoRating | R c  deriving (Show,Eq)

dis :: Eq a => [a] -> [a]
dis []=[];
dis (x:xs)=if((contain x xs)) then (dis xs)else (x:(dis xs))
contain l []=False
contain l (x:xs)= if(l==x) then True else (contain l xs)

fromRatingsToItems :: Eq a => [(b,a,c)] -> [a]
fromRatingsToItems l=(dis (fromRatingsToItemsH l))
fromRatingsToItemsH []=[]
fromRatingsToItemsH ((_,b,_):xs)=(b:(fromRatingsToItemsH xs))

fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsersH []=[]
fromRatingsToUsersH ((a,_,_):xs)=(a:(fromRatingsToUsersH xs))
fromRatingsToUsers  l=(dis (fromRatingsToUsersH l))

hasRating :: (Eq a, Eq b) => b -> a -> [(b,a,c)] -> Bool
hasRating _ _ []=False
hasRating a b ((x,y,c):xs) =if(a==x&&y==b) then True else (hasRating a b xs)

getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating _ _ []= error "No given rating"
getRating a b ((x,y,c):xs) =if(a==x&&y==b) then c else (getRating a b xs)

formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]
formMatrixUser _ [] _ =[]
formMatrixUser a (x:xs) l= if(hasRating a x l) then  (R(getRating a x l)):(formMatrixUser a xs l)else NoRating:(formMatrixUser a xs l)

formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
formMatrix [] _ _=[]
formMatrix (x:xs) b l= (formMatrixUser x b l):(formMatrix xs b l)

getIndex _ []=error"out Of Bound"
getIndex 0 (x:xs)=x
getIndex y (x:xs)= if(y/=0)then (getIndex (y-1) xs ) else error "out Of Bound"

numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem _ []=0
numberRatingsGivenItem x (y:ys)= if((getIndex x y)==NoRating)then (numberRatingsGivenItem x ys)else 1+(numberRatingsGivenItem x ys)

differeneRatings :: Fractional a => Rating a -> Rating a -> a
differeneRatings NoRating _= 0.0
differeneRatings _ NoRating=0.0
differeneRatings (R x) (R y)=x-y

matrixPairs :: (Num a, Ord a) => a -> [(a,a)]
matrixPairs 0=[]
matrixPairs n= foldr(++)[] (helperpair 0 n) 
helperpair start end=if (start< end )then (makeRow2 start 0 end) :(helperpair (start+1) end) else []
makeRow2 i j end =if(j<end)then (i,j):makeRow2 i (j+1) end else []

dMatrix :: Fractional a => [[Rating a]] -> [a]
dMatrix (x:xs)=dMatrixHelper (matrixPairs (length x))  (x:xs)
dMatrixHelper [] l =[]
dMatrixHelper ((i,j):xs) l= (difSum (getIndexinList i l)  (getIndexinList j l)) : (dMatrixHelper xs l)
difSum [] []=0
difSum (x:xs) (y:ys) =(differeneRatings x y)+ (difSum xs ys)
getIndexinList _ []=[]
getIndexinList index (x:xs) =(getIndex index x ):(getIndexinList index xs)

freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]
freqMatrix (x:xs) =freqMatrixHelper (matrixPairs (length x))  (x:xs)
freqMatrixHelper [] l =[]
freqMatrixHelper ((i,j):xs) l= (rate (getIndexinList i l)  (getIndexinList j l)) : (freqMatrixHelper xs l)
rate[] []=0
rate (x:xs) (y:ys) =if(x/=NoRating && y/=NoRating)then 1+ (rate xs ys)else (rate xs ys)

diffFreqMatrix :: Fractional a => [[Rating a]] -> [a] 
diffFreqMatrixHelper [] []=[]
diffFreqMatrixHelper (x:xs) (y:ys) =(x/y):(diffFreqMatrixHelper xs ys)
diffFreqMatrix l = diffFreqMatrixHelper (dMatrix l) (freqMatrix l)

predict (x:xs) u i = if ((hasv (x:xs) u i) ) then (helperPredict (setrate i 0 (getrateList (getIndex u (sprate (x:xs))))) (getrateListofList(removeuserList (sprate (x:xs)) (getuser (getIndex 0 (getIndex u (sprate (x:xs))))))) i)
 else (helperPredict2 (x:xs) u  i)

helperPredict ur er index=(sumall (sumvalues ((divList  (length er) (sumvalues (diffList index er) 0 (length (getIndex 0 er)))):ur:[]) 0 ((length er)+1))) /(fromIntegral ((length ur)-1))
helperPredict2 (x:xs) u i=(fromIntegral( getrate (getIndex i (getIndex u (sprate (x:xs))))))+0.0

hasv (x:xs) u i= if(u>0) then if((length (getIndex 0 (sprate (x:xs))))<=(length (getIndex u (sprate (x:xs)))))then False else True else if((length (getIndex (u+1) (sprate (x:xs))))<=(length (getIndex u (sprate (x:xs)))))then False else True

removeuser [] a=[]
removeuser ((b,c,d):xs) a=if(a==b)then  (removeuser xs a) else ((b,c,d):(removeuser  xs a))
cona ((a,b,c):xs) u=if (a==u)then True else False
removeuserList []  a=[]
removeuserList (x:xs) a=if (cona x a)then xs else x:(removeuserList xs a) 

sprate []=[]
sprate ((a,b,c):xs)= (sprateH a ((a,b,c):xs)):(sprate (removeuser  ((a,b,c):xs) a) )
sprateH a []=[]
sprateH a ((b,c,d):xs)=if(a==b)then  ((b,c,d):(sprateH a xs)) else (sprateH a xs)

divList a []=[]
divList a (x:xs)= x/ (fromIntegral a):(divList a xs)

diff _ []=[]
diff i (x:xs)= (i-x):(diff i xs)
diffList index []=[]
diffList index (x:xs)= (diff (getIndex index x) x):(diffList index xs)

setrate i c []=[c-c]
setrate i c (x:xs)=if (i==c&&(length (x:xs))>0) then ((c-c):x:xs)else (x:(setrate i (c+1) xs))	
getrate (a,b,c)=fromIntegral c
getuser (a,b,c)=a
getrateListofList[]=[]
getrateListofList (x:xs) =(getrateList x):(getrateListofList xs)
getrateList []=[]
getrateList (x:xs) =(getrate x):(getrateList xs)

sum1 [] index=0
sum1 (x:xs) index= (getIndex index x)+(sum1 xs index)
sumall []=0
sumall (x:xs) =x+(sumall xs)
sumvalues (x:xs) index last= if(index<last)then (sum1 (x:xs) index ):(sumvalues (x:xs) (index+1) last )else []