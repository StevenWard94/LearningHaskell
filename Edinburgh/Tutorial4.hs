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
