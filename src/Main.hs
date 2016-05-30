{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.UI.WX hiding (resize)
import Graphics.UI.WXCore.WxcClassesMZ (spinCtrlSetRange)

import Vision.Image
import Vision.Image.Storage.DevIL (Autodetect (..), load, save, StorageError)
import Vision.Primitive 

main :: IO ()
main = start gui

imageFiles = [("Image Files", ["*.bmp", "*.png", "*.jpg", "*.jpeg", "*.gif"])] 
noImage :: Maybe RGB
noImage = Nothing

gui :: IO ()
gui = do
    f <- frame [text := "Honey", clientSize := sz 300 200]
    p <- panel f []
    maybeRgb <- variable [value := noImage ]
    file <- menuPane [text := "&File"]
    open <- menuItem file [text := "&Open image\tCtrl+O", help := "Opens an image"]
    save <- menuItem file [text := "&Save image as ...\tCtrl+S", help := "Saves the resized image"]
    quit <- menuQuit file [help := "Quit Honey", on command := close f]
    hlp <- menuHelp []
    about <- menuAbout hlp [help := "About Honey"]
    status <- statusField [text := "Welcome to Honey"]
    aspectRatioBox <- checkBox p []
    widthInput <- spinCtrl p 1 1280 []
    heightInput <- spinCtrl p 1 1024 []
    set widthInput [on select := setDimensions maybeRgb widthInput heightInput aspectRatioBox "height"]
    set heightInput [on select := setDimensions maybeRgb widthInput heightInput aspectRatioBox "width"]
    resizeButton <- button p [text := "Resize", on command := rsz widthInput heightInput maybeRgb status]
    set f [ layout := container p
                    $ margin 10
                    $ column 5 [boxed "new dimensions" (grid 5 5 [[label "keep aspect ratio", hfill $ widget aspectRatioBox]
                                                                 ,[label "width", hfill $ widget widthInput]
                                                                 ,[label "height", hfill $ widget heightInput]])
                               , floatBottomRight $ row 5 [widget resizeButton]]
          , statusBar := [status]
          , menuBar := [file, hlp]
          , on (menu open) := loadImage f maybeRgb widthInput heightInput status
          , on (menu save) := saveImage f maybeRgb status
          , on (menu about) := infoDialog f "About Honey" "First shot"
          ]

    where

        setDimensions maybeRgb widthInput heightInput aspectRatioBox what = do 
            checked <- get aspectRatioBox checked
            if checked then do
                img <- get maybeRgb value
                case img of 
                    Nothing -> return ()
                    Just (rgb::RGB) -> do
                        let (Z :. h) :. w = shape rgb
                        if what == "height" then do -- widthInput has changed
                            newWidth <- get widthInput selection
                            let changeFactor = fromIntegral newWidth / fromIntegral w
                            set heightInput [selection := round $ changeFactor * fromIntegral h] 
                        else do -- heightInput has changed
                            newHeight <- get heightInput selection
                            let changeFactor = fromIntegral newHeight / fromIntegral h
                            set widthInput [selection := round $ changeFactor * fromIntegral w] 
            else 
                return ()

        loadImage frame maybeRgb widthInput heightInput status = do 
            Just filepath <- fileOpenDialog frame True True "Open Image" imageFiles "" "" 
            image <- load Autodetect filepath
            case image of
                Left (err) -> do
                    set status [text := "Error loading image: " ++ (show err)]
                Right (rgb::RGB) -> do
                    set status [text := "Successfully loaded image " ++ filepath]
                    -- update controls
                    let (Z :. h) :. w = shape rgb
                    spinCtrlSetRange widthInput 1 w
                    set widthInput [selection := w]
                    spinCtrlSetRange heightInput 1 h
                    set heightInput [selection := h]
                    -- store the image
                    set maybeRgb [value := Just rgb]
            return ()

        saveImage frame maybeRgb status = do 
            image <- get maybeRgb value
            case image of 
                Nothing -> set status [text := "Error: no image in stock"]
                Just img -> do
                    maybeFilePath <- fileSaveDialog frame True True "Save Image" imageFiles "" "" 
                    case maybeFilePath of
                        Just filepath -> do
                            result <- save Autodetect filepath img
                            case result of 
                                Nothing -> set status [text := "Successfully saved image"]
                                Just err -> set status [text := "Error saving image: " ++ (show err)]
                        Nothing -> return ()
            return ()

        rsz widthInput heightInput maybeRgb status = do 
            width <- get widthInput selection
            height <- get heightInput selection
            image <- get maybeRgb value
            case image of
                Nothing -> return ()
                Just (img::RGB) -> do
                    let resized = resize Bilinear (ix2 height width) img
                    set maybeRgb [value := Just resized]
                    set status [text := "Resized image to " ++ (show width) ++ " x " ++ (show height)]
                    return ()
    
