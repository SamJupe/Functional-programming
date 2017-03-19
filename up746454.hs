-- 
-- MATHFUN
--UP746454

--
import Data.List hiding (delete)
import Data.Set hiding (foldr, map, filter)
import Text.Printf

--
-- Types
type Title    = String
type Director = String
type Year     = Int
type Fans     = String

--
-- Define Film type here
data Film    = Film Title Director Year [Fans]
                deriving (Show,Read,Eq,Ord)

testDatabase :: [Film]
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Sam", "Olga", "Tim"],
                Film "The Fly" "David Cronenberg" 1986 ["Garry", "Dave", "Zoe", "Kevin", "Emma"],
                Film "Body Of Lies" "Ridley Scott" 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"],
                Film "Avatar" "James Cameron" 2009 ["Dave", "Amy", "Liz"],
                Film "Titanic" "James Cameron" 1997 ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"],
                Film "The Departed" "Martin Scorsese" 2006 ["Wally", "Liz", "Kevin", "Tim", "Emma"],
                Film "Aliens" "Ridley Scott" 1986 ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"],
                Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo", "Wally", "Emma"],
                Film "Prometheus" "Ridley Scott" 2012 ["Kevin", "Tim", "Emma", "Jo", "Liz"],
                Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave", "Amy", "Garry", "Ian", "Neal"],
                Film "Bridge Of Spies" "Steven Spielberg" 2015 ["Wally", "Sam", "Dave", "Neal"],
                Film "Jaws" "Steven Spielberg" 1975 ["Dave", "Jo", "Zoe", "Wally", "Emma", "Kate"],
                Film "The Martian" "Ridley Scott" 1975 ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"],
                Film "The BFG" "Steven Spielberg" 2016 ["Sam", "Wally", "Dave", "Jo", "Kate"],
                Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe"],
                Film "Gladiator" "Ridley Scott" 2000 ["Olga", "Neal", "Kate", "Heidi", "Bill", "Sam", "Zoe"],
                Film "The Green Mile" "Frank Darabont" 1999 ["Kevin", "Tim", "Emma", "Heidi"],
                Film "True Lies" "James Cameron" 1994 ["Sam", "Dave"],
                Film "Super 8" "J J Abrams" 2011 ["Kevin", "Tim", "Emma", "Olga", "Heidi"],
                Film "Minority Report" "Steven Spielberg" 2002 ["Kevin", "Kate", "Tim", "Emma", "Olga", "Jenny", "Zoe"],
                Film "War Horse" "Steven Spielberg" 2011 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"],
                Film "Silence" "Martin Scorsese" 2016 ["Wally", "Emma", "Tim", "Heidi", "Bill", "Olga", "Jo"],
                Film "The Terminal" "Steven Spielberg" 2004 ["Kate", "Dave", "Jo", "Wally", "Emma"],
                Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz", "Jo"],
                Film "Hugo" "Martin Scorsese" 2011 ["Wally","Sam"]]

--helpers
filmToString :: Film -> String
filmToString (Film title director year fans) = "\n Title: " ++ title ++ "\n Director: " ++ director ++ "\n Year: " ++ show(year) ++ "\n Fans:" ++ show(length fans)
--Functions to get single item in film in database

listToString :: [String] -> String
listToString [] = ""
listToString (x:xs) = x ++ ", " ++ listToString xs

getFilmsFans :: [Film] -> [Fans]
getFilmsFans [(Film _ _ _ fans)] = fans


getFilmByTitle :: Title -> [Film] -> [Film]
getFilmByTitle titleToFilter = filter(\(Film title director year fans) -> title == titleToFilter)

getDirectorsFilms :: Director -> [Film] -> [Film]
getDirectorsFilms directorToFilter = filter(\(Film title director year fans) -> director == directorToFilter)

getFilmsOnYear :: Year -> [Film] -> [Film]
getFilmsOnYear yearToFilter = filter(\(Film title director year fans) -> year == yearToFilter)

getFilmsAfterYear :: Year -> [Film] -> [Film]
getFilmsAfterYear yearToFilter = filter(\(Film title director year fans) -> yearToFilter < year)

getFanOfFilms :: Fans -> [Film] -> [Film]
getFanOfFilms fanToFilter = filter(\(Film title director year fans) -> fanToFilter `elem` fans)
--
-- functions getting lists in film in database
getDirectorsFans :: [Film] -> [Fans]
getDirectorsFans [] = []
getDirectorsFans ((Film title director year fans):films) = fans ++ getDirectorsFans films

filmExists :: [Film] -> Title -> Bool
filmExists [] title = False
filmExists ((Film title director year fans):films) titleToCheck = if titleToCheck == title
                                                                 then True 
                                                                 else filmExists films titleToCheck

fanExists :: [Fans] -> Fans -> Bool
fanExists  [] fans = False
fanExists (x:xs) fansToCheck = if fansToCheck == x then True else fanExists xs fansToCheck

getFilmsDirectors :: [Film] -> [Director]
getFilmsDirectors [] = []
getFilmsDirectors ((Film title director year fans):films) = [director] ++ getFilmsDirectors films

getOccurancesDirectors :: Director -> [Director] -> Int
getOccurancesDirectors x xs = (length . filter (== x)) xs

getAllOccurancesDirectors :: [Director] -> [Director] -> [(Director, Int)]
getAllOccurancesDirectors [] test = []
getAllOccurancesDirectors (x:[]) test = [(x,getOccurancesDirectors x test)]
getAllOccurancesDirectors (x:xs) test = [(x,getOccurancesDirectors x test)] ++ getAllOccurancesDirectors xs test

occurancesToString :: (Director, Int) -> String
occurancesToString (director, numOfOccurances) = "\n Director: " ++ director ++ "\n Number Of films: " ++show(numOfOccurances)
--
--  Your functional code goes here


addFilm :: Title -> Director -> Year -> [Film] -> [Film]
addFilm title director year filmDB = filmDB ++ [Film title director year []]


databaseToString :: [Film] -> String
databaseToString filmDb = (unlines.map filmToString) filmDb

filmsAfterYear :: Year -> [Film] -> String
filmsAfterYear yearToFilter db = databaseToString (getFilmsAfterYear yearToFilter db)

filmsFans :: Title -> [Film] -> String
filmsFans filmTitle db = listToString(getFilmsFans(getFilmByTitle filmTitle db))

fanOfFilms :: Fans -> [Film] -> String
fanOfFilms fanToFilter db = databaseToString (getFanOfFilms fanToFilter db)

userIsFan :: Fans -> Title -> [Film] -> [Film]
userIsFan fan filmTitle = map(\(Film title director year fans) -> if title == filmTitle && fan `notElem` fans
                                                                then (Film title director year (fan : fans)) 
                                                                else (Film title director year fans))
directorsFans :: Director -> [Film] -> String
directorsFans director db = listToString( nub (getDirectorsFans db))

allDirectors :: Fans -> [Film] -> String
allDirectors fanToFilter filmdb = (unlines.map occurancesToString) (getAllOccurancesDirectors (nub (listOfFansDirectors)) (listOfFansDirectors))
    where listOfFansDirectors = getFilmsDirectors(getFanOfFilms fanToFilter filmdb)
-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

--demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2017 film "Alien: Covenant"
--                   by "Ridley Scott" to testDatabase
--demo 2  = putStrLn (filmsAsString testDatabase)
--demo 3  = putStrLn all films released after 2008
--demo 4  = putStrLn all films that "Liz" is a fan of
--demo 5  = putStrLn all fans of "Jaws"
--demo 6  = putStrLn all films after "Liz" says she becomes fan of "The Fly"
--demo 6 = putStrLn all films after "Liz" says she becomes fan of "Avatar"
--demo 7 =  putStrLn all fans of films directed by "James Cameron"
--demo 8  = putStrLn all directors & no. of their films that "Liz" is a fan of

--
--
-- Your user interface code goes here
--
--
main :: IO ()
main = do films <- readFile "films.txt"
          let filmDB = read films :: [Film]
          putStrLn $ databaseToString filmDB
          putStrLn "Enter your name: "
          name <- getLine
          filmDB <- userInterface filmDB name
          writeFile "films.txt" (show filmDB)

userInterface :: [Film] -> String -> IO [Film]
userInterface db name = do
        putStrLn "\n Menu"
        putStrLn "1 - Add a new film to the database"
        putStrLn "2 - show films in the database"
        putStrLn "3 - Give all the films that released after a particular year"
        putStrLn "4 - Give all films that particular user is a fan of"
        putStrLn "5 - Give all the fans of a particular film"
        putStrLn "6 - Become a fan of a film"
        putStrLn "7 - Give all fans of a particular director"
        putStrLn "8 - List all directors"
        putStrLn "9 - Save and exit"
        input <- getLine
        if input /= "0"
            then case input of
                "1" -> do
                    putStrLn "Enter Title of film: "
                    filmName <- getLine
                    if (filmExists db filmName) then do
                        putStrLn "Unable to add this film to database. Title already exists."
                        userInterface db name
                                else do
                                    putStrLn "Enter the Directors name: "
                                    directorName <- getLine
                                    putStrLn "Enter the year the film was released: "
                                    getYear <- getLine
                                    let year = read getYear :: Year 
                                    putStrLn "Film has been added"
                                    userInterface (addFilm filmName directorName year db) name
                
                "2" -> do
                    putStrLn $ databaseToString db
                    userInterface db name
                "3" -> do 
                    putStrLn "Enter The year: "
                    getYear <- getLine
                    let year = read getYear :: Year 
                    putStrLn $ filmsAfterYear year db  
                    userInterface db name
                "4" -> do
                    putStrLn $ fanOfFilms name db
                    userInterface db name
                "5" -> do
                    putStrLn "Enter The films Title: "
                    filmName <- getLine
                    if(filmExists db filmName) then do
                        putStrLn $ filmsFans filmName db
                        userInterface db name
                    else do
                        putStrLn "The film does not exist in the database."
                        userInterface db name
                "6" -> do
                    putStrLn "Enter Film name you are a fan of: "
                    filmName <- getLine
                    if (filmExists db filmName) then do
                        if fanExists (getFilmsFans (getFilmByTitle  filmName db)) name then do
                            putStrLn "You are already a fan!"
                            userInterface db name
                        else do
                            putStrLn "added you as fan" 
                            userInterface (userIsFan name filmName db ) name
                                else do
                                    putStrLn "Unable to add you as a fan because film does not exist in database."
                                    userInterface db name
                "7" -> do
                    putStrLn "Enter Directors name: "
                    directorsName <- getLine
                    putStrLn $ directorsFans directorsName db
                    userInterface db name
                "8" -> do
                    putStrLn $ allDirectors name db
                    userInterface db name
            else
                return db

                                