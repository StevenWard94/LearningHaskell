-- vim: fdl=1:
-- |
-- Module:        Edinburgh.Tutorial4
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 17
--

-- This module consists of my (attempted) solutions to the fourth Edinburgh Tutorial:
--                            SCREEN-SCRAPING
--           Informatics 1 - Functional Programming: Tutorial 4
--
-- the instructions for which can be found at:
--      http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#haskell
--
--------------------------------------------------------------------------------
--
module Edinburgh.Tutorial4 where

import Test.QuickCheck
import Network.HTTP ( simpleHTTP, getRequest, getResponseBody )

import Data.List ( nub, tails )
import Data.Char

import Control.Monad ( liftM2 )

-- Basic Screen Scraper \begin

-- INTRODUCTION \begin
-- A "screen scraper" is a tool used to extract data from web sites via their
-- source code. In this exercise, you will implement one of the most hated
-- screen scrapers: one that extracts email addresses. However, this exercise
-- will show you a "useful" purpose of the email screenscraper!
-- We are going to be extracting names and email addresses from web pages
-- written in HTML. For instance, from the HTML source contained in 'testHTML'
-- (from - hypothetically - the website located at 'testURL'), we are going to
-- extract a list of the '<a>' elements. If the 'href' attribute of an element
-- begins with "http:", then it is the address of a web page; if it begins with
-- "mailto:", then it is an email address. In the case of 'testHTML', the links
-- extracted would be those contained in 'testLinks'. From this list of links,
-- we can then develop an "address book" of names and email addresses.
-- \end

-- TYPE SYNONYMS and SAMPLE DATA \begin

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

testURL :: URL
testURL = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: HTML
testHTML =   "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring Test Page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]

testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella", "dts@inf.ed.ac.uk"), ("Karoliina Lehtinen", "m.k.lehtinen@sms.ed.ac.uk") ]

-- \end

-- SYSTEM INTERACTION \begin

getURL :: URL -> IO String
getURL = (getResponseBody =<<) . simpleHTTP . getRequest

--emailsFromURL :: URL -> IO ()
--emailsFromURL url = do
--                      html <- getURL url
--                      let emails = emailsFromHTML html
--                      putStr $ ppAddrBook emails

--emailsByNameFromURL :: URL -> Name -> IO ()
--emailsByNameFromURL url name = do
--                                 html <- getURL url
--                                 let emails = emailsByNameFromHTML html name
--                                 putStr $ ppAddrBook emails

-- \end

-- EXERCISES \begin

-- Exercise 1:
-- Write a function, sameString, that returns True when two strings are the same
-- (case-insensitive). WARNING: Unintuitively, the mapping between upper and
-- lowercase characters is not one-to-one. For example, the greek letter, μ, and
-- the "micro" sign both map to the same uppercase letter. What does your code
-- do on 'sameString "\181" "\956"'? In this case, either behavior is acceptable
-- as long as the tests don't fail for input containing these characters!
sameString :: String -> String -> Bool
sameString str1 str2 =
    map (toLower . toUpper) str1 == map (toLower . toUpper) str2


-- Exercise 2:
-- Write a function, prefix, that determines whether a given string is a prefix
-- of another given string, much like the library function 'isPrefixOf', except
-- that this function should be case-insensitive. Then, check the function using
-- the predefined (or your own equivalent) test property, 'prop_prefix'.
prefix :: String -> String -> Bool
prefix = liftM2 (.) sameString $ take . length

prop_prefixPos :: String -> Int -> Bool
prop_prefixPos str n =
    let substr = take n str
     in prefix substr (map toLower str) && prefix substr (map toUpper str)

prop_prefixNeg :: String -> Int -> Bool
prop_prefixNeg str n =
    let substr = take n str
     in sameString str substr || (not $ prefix str substr)


-- Exercise 3
--  (a) Write the function, contains, as in tutorial 2 but case-insensitive.
--  (b) Write a test property, prop_contains, to test your 'contains' function.
contains :: String -> String -> Bool
contains str substr = any (prefix substr) (tails str)

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n m =
    let substr = take n $ drop m str
     in map toUpper str `contains` substr &&
        map toLower str `contains` substr


-- Exercise 4 \begin
-- (a) Write a case-insensitive function, takeUntil, that takes two strings and returns the
-- contents of the second string BEFORE the first occurrence of the first
-- string. If the second string does not contain the first string, return the
-- entire second string.
takeUntil :: String -> String -> String
takeUntil substr str = case str of
                         []     -> ""
                         (c:cs) | substr `prefix` str -> ""
                                | otherwise           -> c : takeUntil substr cs

-- (b) Write a case-insensitive function, dropUntil, that returns the contents
-- of the second string AFTER the first occurrence of the first string. If the
-- second string does not contain the first string, return an empty string.
dropUntil :: String -> String -> String
dropUntil substr str
  | null str            = ""
  | substr `prefix` str = drop (length substr) str
  | otherwise           = dropUntil substr $ tail str

-- \end

-- Exercise 5 \begin

-- 5. (a) Write a case-insensitive function, split, that divides the second
-- argument at every occurrence of the first, returning the resulting strings as
-- a list. The result should NOT CONTAIN the delimiter and the function should
-- return an error if the delimiter argument is an empty list.
split :: String -> String -> [String]
split delim str
  | null delim           = error "Delimiter cannot be an empty string."
  | str `contains` delim = takeUntil delim str : split delim (dropUntil delim str)
  | otherwise            = [str]


-- 5. (b) Write a function, reconstruct, that reverses the result of 'split'.
-- That is, it should take a string and a list of strings, and return the string
-- created by joining the list of strings with the string argument in between
-- each string (but not at the beginning or end of the resulting string)
reconstruct :: String -> [String] -> String
reconstruct delim = foldr1 $ \cs ds -> cs ++ delim ++ ds


-- 5. (c) Use the predefined (or your own equivalent) test function, prop_split,
-- to test your 'split' function.
prop_split :: Char -> String -> String -> Bool
prop_split ch delim str = let del = ch : delim
                           in reconstruct del (split del str) `sameString` str
-- \end

-- Exercise 6:
-- Use your 'split' function to write a function, linksFromHTML. You can assume
-- that a link begins with the string, '<a href="'. DO NOT include the separator
-- in your results and DO NOT include the HTML that precedes the first link. Use
-- the predefined (or your own equivalent) NON-QUICKCHECK test function,
-- testLinksFromHTML, to test your function on the sample data provided in 'testHTML'.
linksFromHTML :: HTML -> [Link]
linksFromHTML = tail . split "<a href=\""

testLinksFromHTML :: Bool
testLinksFromHTML = linksFromHTML testHTML == testLinks


-- Exercise 7:
-- Write a function, takeEmails, that extracts just the email addresses from
-- a list returned by 'linksFromHTML'.
takeEmails :: [Link] -> [Link]
takeEmails = filter $ prefix "mailto:"


-- Exercise 8:
-- Write a function, link2pair, which converts a "mailto" link into a name and
-- corresponding email address pair. The "name" is the plain-text between the
-- <a ...> and </a> tags; the email address is the value of the 'href'
-- attribute without "mailto:". Add an appropriate error message if the link
-- provided is not a "mailto" link. Example:
--     *Main> link2pair "mailto:john@smith.co.uk\">John</a>"
--     ("John","john@smith.co.uk")
link2pair :: Link -> (Name,Email)
link2pair link = let email = takeUntil "\">" $ dropUntil "mailto:" link
                     name  = takeUntil "</a>" $ dropUntil "\">" link
                     errmsg = "link2pair: link element must contain an email address"
                  in if link `contains` "mailto:"
                        then (name,email)
                        else error errmsg


-- Exercise 9:
-- Combine your 'linksFromHTML', 'takeEmails', and 'link2pair' functions to
-- define 'emailsFromHTML, which extracts all "mailto" links from a web page's
-- HTML source and returns a list of (Name,Email) pairs with all duplicates
-- removed. Then, test your function with 'testEmailsFromHTML'.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML = nub . map link2pair . takeEmails . linksFromHTML

testEmailsFromHTML :: Bool
testEmailsFromHTML = emailsFromHTML testHTML == testAddrBook

--     \end
-- \end

-- Pulling In Live URLs \begin
-- Over the next few exercises, you will be implementing a function,
-- emailsByNameFromURL, which will search for, and extract, the email address of
-- a person whose name you know.

-- EXERCISES \begin

-- Exercise 10:
-- Write a function, findEmail, which, given (part of) a name and list of
-- (Name,Email) pairs, returns the sublist of pairs that matched the name.
findEmail :: Name -> [(Name,Email)] -> [(Name,Email)]
findEmail query addrBook = [ (name,email) | (name,email) <- addrBook, name `contains` query ]


-- Exercise 11:
-- Define the function, emailsByNameFromHTML, which takes an HTML string and
-- returns a list of (Name,Email) pairs extracted from the HTML that contain the
-- name (or part of a name) provided.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML = flip findEmail . emailsFromHTML

--     \end
-- \end

-- Optional Material \begin

-- SEARCHING FOR STRINGS \begin
-- In the previous section, you have written functions to find email addresses
-- belonging to people whose names contain the input string. Now, you will write
-- code to select names that match more elaborate criteria.

-- Exercise 12:
-- Write a function, hasInitials, which returns True if the initials of the name
-- provided are EXACTLY the initials provided.
hasInitials :: String -> Name -> Bool
hasInitials [] [] = True
hasInitials [] _  = False
hasInitials _  [] = False
hasInitials init name
  | (toLower . head) init == (toLower . head) name =
          hasInitials (tail init) $ dropUntil " " name
  | otherwise                                     = False


-- Exercise 13:
-- Write a function, emailsByMatchFromHTML, which should extract and return
-- email address belonging to people whose names fulfill the criterion specified
-- by the first argument. Then, write a function, emailsByInitialsFromHTML,
-- which does the same thing but always matching on 'hasInitials'.
takeEmailsWith :: (Name -> Bool) -> [(Name,Email)] -> [(Name,Email)]
takeEmailsWith _ []            = []
takeEmailsWith p ((n,e):addrs) = if p n
                                    then (n,e) : takeEmailsWith p addrs
                                    else takeEmailsWith p addrs

emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name,Email)]
emailsByMatchFromHTML = (. emailsFromHTML) . takeEmailsWith

emailsByInitialsFromHTML :: String -> HTML -> [(Name,Email)]
emailsByInitialsFromHTML initials = emailsByMatchFromHTML $ hasInitials initials


-- Exercise 14:
-- Write a function, myCriteria, which tests whether a name matches a criterion
-- of your choice. Then, use this function along with the previous functions to
-- define 'emailsByMyCriteriaFromHTML'.
myCriteria :: [String] -> (Name,Email) -> Bool
myCriteria _  ("","") = True
myCriteria [] _  = False
myCriteria rmv (name,email) =
    case length rmv of
      1 -> if name `contains` head rmv then True else False
      2 | (null . head) rmv -> if email `contains` head rmv then True else False
        | otherwise         -> myCriteria [head rmv] (name,email) || myCriteria ([""] ++ tail rmv) (name,email)
      _ -> myCriteria (take 2 rmv) (name,email)
