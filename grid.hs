module Main where
import Graphics.Gloss
import Graphics.Gloss.Data.Picture

import Lib

main :: IO ()
main = display (InWindow "Minesweeper" (700, 700) (0, 0)) green grid
    where
    grid = 
        color black (line [ (-500, -350), (-500, 350) ]) <>
        color black (line [ (-450, -350), (-450, 350) ]) <>
        color black (line [ (-400, -350), (-400, 350) ]) <>
        color black (line [ (-350, -350), (-350, 350) ]) <> --[ (-350, -350), (-350, 350) ]) <>
        color black (line [ (-300, -350), (-300, 350) ]) <>
        color black (line [ (-250, -350), (-250, 350) ]) <>
        color black (line [ (-200, -350), (-200, 350) ]) <>
        color black (line [ (-150, -350), (-150, 350) ]) <>
        color black (line [ (-100, -350), (-100, 350) ]) <>
        color black (line [ (-50, -350), (-50, 350) ]) <>
        color black (line [ (0, -350), (0, 350) ]) <>
        color black (line [ (50, -350), (50, 350) ]) <>
        color black (line [ (100, -350), (100, 350) ]) <>
        color black (line [ (150, -350), (150, 350) ]) <>
        color black (line [ (200, -350), (200, 350) ]) <>
        color black (line [ (250, -350), (250, 350) ]) <>
        color black (line [ (300, -350), (300, 350) ]) <>
        color black (line [ (350, -350), (350, 350) ]) <> --[ (350, -350), (350, 350) ]) <>
        color black (line [ (400, -350), (400, 350) ]) <>
        color black (line [ (450, -350), (450, 350) ]) <>
        color black (line [ (500, -350), (500, 350) ]) <>
        color black (line [ (-500, -350), (500, -350) ]) <> --color black (line [ (-350, -350), (350, -350) ]) <>
        color black (line [ (-500, -300), (500, -300) ]) <>
        color black (line [ (-500, -250), (500, -250) ]) <>
        color black (line [ (-500, -200), (500, -200) ]) <>
        color black (line [ (-500, -150), (500, -150) ]) <>
        color black (line [ (-500, -100), (500, -100) ]) <>
        color black (line [ (-500, -50), (500, -50) ]) <>
        color black (line [ (-500, 0), (500, 0) ]) <>
        color black (line [ (-500, 50), (500, 50) ]) <>
        color black (line [ (-500, 100), (500, 100) ]) <>
        color black (line [ (-500, 150), (500, 150) ]) <>
        color black (line [ (-500, 200), (500, 200) ]) <>
        color black (line [ (-500, 250), (500, 250) ]) <>
        color black (line [ (-500, 300), (500, 300) ]) <>
        color black (line [ (-500, 350), (500, 350) ]) --color black (line [ (-350, 350), (350, 350) ])