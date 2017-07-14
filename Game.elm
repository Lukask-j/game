type Msg = Tick Float GetKeyState
         | StartGame
         | SubmitAnswer Answer Answer
         | Reset
         | GoInstructions

type GameState = MainMenu
               | InGame
               | EndOfGame
               | Failure
               | Instructions

type Answer = A | B | C | D

main =
    gameApp Tick {   model = init
                 ,   view = view
                 ,   update = update
                 }
myshapes = group [
                mastersword
                
                ]


--- MODEL ---

init = { state = MainMenu
       , levels = [ level1,
                    level2,
                    level3,
                    level4,
                    level5,
                    level6,
                    level7,
                    level8,
                    level9,
                    level10
                  ]
       , chances = 2
       , time = 0  -- This is specifically ANIMATION time.

         -- Below are a set of variables that aren't used in the template but
         -- maybe you can figure out how to use them? You can add more too!
       , score = 0
       , timelimit = 20
       , highscore = 0
       , current = 0
       }

--- VIEW ---

view model = case model.state of
                MainMenu -> collage 1000 800 (menuView model)
                InGame   -> collage 1000 600 (levelView (List.head model.levels) model.time model.chances model.timelimit model.score)
                EndOfGame   -> collage 1000 1000 (endView model)
                Failure  -> collage 1000 600 (failView model)
                Instructions -> collage 1000 600 (instructionsView model)
instructionsView model = [ group [powerbutton |> scale 2.5
                            |> move (300,-150)
                         , text "START"
                            |> size 31.25
                            |> centered
                            |> filled (rgb 21 243 5)
                            |> move (300,-170)
                         , text "How to play"
                            |> size 60
                            |> centered
                            |> filled green
                            |> move (0,200)
                         , text "click on the answer you think is right and make it through the questions with"
                            |> size 30
                            |> centered
                            |> filled black
                            |> move (0,150)
                         , text "your alotted 3 lives"
                            |> size 30
                            |> centered
                            |> filled black
                            |> move (0,120)
                         , text "with every wrong answer you will lose a life"
                            |> size 30
                            |> centered
                            |> filled black
                            |> move (0,90)
                         , text "you will gain points for every right answer"
                            |> size 30
                            |> centered
                            |> filled black
                            |> move (0,60)
                         , text "all subject matter is related to a video game"
                            |> size 30
                            |> centered
                            |> filled black
                            |> move (0,30)
                         , text "good luck"
                            |> size 30
                            |> centered
                            |> filled black
                            |> move (0,0)
                         ] |> notifyMouseDown StartGame
                         ]

menuView model = [ group [ powerbutton |> scale 4
                         , text "START"
                            |> size 50
                            |> centered
                            |> filled (rgb 21 243 5)
                            |> move (0,-40)
                            , text "Flick Flurry"
                            |> size 50
                            |> centered
                            |> filled black
                            |> move (0,260)
                            , text "By: Noah & Lukas"
                                                |> filled black
                                                |> move (-115,230)
                                                
                                                |> scale 2.5
                         ] |> notifyMouseDown StartGame
                         ,  instructionsbutton1 |> scale 1
                         |> move (0,-20)
                         |> notifyMouseDown GoInstructions
                 ]

endView model = [ group [ powerbutton1 |> scale 4
                         , text "RESET"
                            |> size 50
                            |> centered
                            |> filled (rgb 255 255 0)
                            |> move (0,-40)
                            , text (toString model.score)
                            |> size 75
                            |> filled black
                            |> move (-65, 250)
                            , text "Your score:"
                            |> size 75
                            |> filled black
                            |> move (-175,350)
                            , text "You Win"
                            |> size 75
                            |> centered
                            |> filled (rgb 21 243 5)
                            |> move (0,-260)
                            
                            ] |> notifyMouseDown Reset
                 ]

failView model = [ group [ powerbutton2 |> scale 4
                         , text "RESET"
                            |> size 50
                            |> centered
                            |> filled (rgb 255 14 0)
                            |> move (0,-40)
                            , text "You Lose"
                            |> size 75
                            |> centered
                            |> filled (rgb 255 0 0)
                            |> move (0,-260)
                         ] |> notifyMouseDown Reset
                 ]

levelView level t chances timelimit score = case level of
                                 Nothing -> []
                                 Just lev ->  [ group (lev.image t)
                                            , option A lev.optionA
                                                |> move (-150,-200)
                                                |> notifyMouseDown (SubmitAnswer A lev.answer)
                                            , option B lev.optionB
                                                |> move (-150,-240)
                                                |> notifyMouseDown (SubmitAnswer B lev.answer)
                                            , option C lev.optionC
                                                |> move (150,-200)
                                                |> notifyMouseDown (SubmitAnswer C lev.answer)
                                            , option D lev.optionD
                                                |> move (150,-240)
                                                |> notifyMouseDown (SubmitAnswer D lev.answer)
                                            , text "Extra chances"
                                                |> filled red
                                                |> move (200,100)
                                                |> scale 1.5
                                            , group (displayChances chances)
                                                |> move (200,150)
                                            , text (toString (ceiling timelimit))
                                                |> size 50
                                                |> filled black
                                                |> move (-340,150)
                                            , text (toString (ceiling score))
                                                |> filled black
                                                |> scale 2.5
                                                |> move (340,0)
                                            , text "Score:"
                                                |> filled blue
                                                |> move (250,0)
                                                |> scale 2.5
                                                
                                            
                                           ]

displayChances chances = case chances of
                            0 -> []
                            _ -> [heart red
                                    |> scale (0.5)
                                    |> move (0 +  chances * 100,0) ] ++ (displayChances (chances - 1))

option ans tex = group [ rectangle 200 30
                            |> filled orange
                       , text ((toString ans) ++ ": " ++ tex)
                            |> size 20
                            |> filled white
                            |> move (-90,-7) ]


level1 = { image = level1_image
         , optionA = "Legend of Zelda"
         , optionB = "Pokemon"
         , optionC = "Mario"
         , optionD = "PacMan"
         , answer = D
         }

level1_image t = [ pacman yellow t |> scale (3)
                    |> move (0,40) ]


level2 = { image = level2_image
         , optionA = "Salt and Sanctuary"
         , optionB = "Bloodborne"
         , optionC = "Legend of Zelda"
         , optionD = "Fire Emblem"
         , answer = C
         
         }

level2_image t = [ mastersword |> scale (3) |> rotate (degrees 180) |> move (0, 0 + 100 * abs(sin(t/50))) 
 ,pedastal |> scale 20 |> move (-100,-170)]

level3 = { image = level3_image
         , optionA = "Digimon"
         , optionB = "pokemon"
         , optionC = "Banjo kazooie"
         , optionD = "Pocket card Jockey"
         , answer = B
         }

level3_image t = [ pokeball |> rotate (abs (sin (t/20)) + 5.75)
  |> move (0,40)
  |> scale 4]

level4 = { image = level4_image
         , optionA = "Portal"
         , optionB = "Garry's mod"
         , optionC = "Ark Survival Evolved"
         , optionD = "Half Life"
         , answer = D
         }

level4_image t = [ halflife |> scale (abs (sin (t/20)) + 3)
                  |> move (50,0)
                               ]
level5 = { image = level5_image
         , optionA = "Mario"
         , optionB = "Sonic"
         , optionC = "Smash Bros"
         , optionD = "Pokemon"
         , answer = C
         }

level5_image t = [ smashlogo |> scale (abs (sin (t/20)) + 3)
                  |> move (50,0)
                               ]
level6 = { image = level6_image
         , optionA = "Counter Strike"
         , optionB = "Team Fortress 2"
         , optionC = "Call of Duty"
         , optionD = "Garry's mod"
         , answer = B
         }

level6_image t = [ teamfortress2 |> scale (abs (sin (t/20)) + 2)
                  |> move (50,0)
                               ]
level7 = { image = level7_image
         , optionA = "Mario"
         , optionB = "Sonic"
         , optionC = "Minecraft"
         , optionD = "Mega Man"
         , answer = A
         }

level7_image t = [ mario |> scale (abs (sin (t/20)) + 2)
                  |> move (50,0)
                               ]
level8 = { image = level8_image
         , optionA = "Portal"
         , optionB = "Mega Man"
         , optionC = "Kirby"
         , optionD = "Princess Peach"
         , answer = C
         }

level8_image t = [ kirby |> scale (abs (sin (t/20)) + 3)
                  |> move (50,0)
                               ]
level9 = { image = level9_image
         , optionA = "Origin"
         , optionB = "Xbox"
         , optionC = "Steam"
         , optionD = "Atari"
         , answer = D
         }

level9_image t = [ atari |> scale (abs (sin (t/20)) + 3)
                  |> move (0,-150)
                               ]   
level10 = { image = level10_image
         , optionA = "Nintendo"
         , optionB = "Steam"
         , optionC = "Origin"
         , optionD = "Playstation"
         , answer = B
         }

level10_image t = [ steam |> scale (abs (sin (t/20)) + 2)
                  |> move (50,0)
                               ]  
                               
                               
heart c = group [circle 50
            |> filled c
            |> move (0,50)
         ,
          circle 50
            |> filled c
            |> move (50,0)
         ,
          square 100
            |> filled c ] |> rotate (degrees 45)
            
            
smashlogo = group [ circle 30
  |> filled (rgb 255 0 0)
  ,rect 60 5
  |> filled white
  |> move (0,-7)
  ,rect 15 60
  |> filled white
  |> move (-13,0)
  ]
  
teamfortress2 = group [ circle 60
                      |> filled (rgb 191 137 62)
                      ,circle 17
                      |> filled white
                      ,rectangle 10 15
                      |> filled white
                      |> rotate (degrees 85)
                      |> scaleY 10
                      ,rectangle 10 15
                      |> filled white
                      |> rotate (degrees -5)
                      |> scaleY 10 ]
steam = group [ circle 50
              |> filled black
               , circle 12
              |> outlined (solid 4) white
              |> move (-16,-18.25)
              , polygon [(0.5,4),(1.5,10),(6.5,10),(8,4)]
              |> filled white
              |> rotate (degrees 135)
              |> scale 4
              |> move (30,5.75)
              , circle 10
              |> filled black
              |> move (-16,-18.75)
             , roundedRect 75 15 7.5
              |> filled white
              |> move (-45,-8)
              |> rotate (degrees -20)
              , circle 19
              |> filled white
              |> move (14,14)
              , circle 13
              |> filled black
              |> move (14,14)
              , circle 10
              |> filled white
              |> move (14,14)
              , curve (0,0) [Pull (3,0) (5,7)]
              |> filled black
              |> rotate (degrees 75)
              |> move (-6.75,-15.5)
              ]

atari = group [ rect 10 6.25
               |> filled (rgb 221 116 51)
               |> move (0,50)
               ,rect 10 6.25
               |> filled (rgb 252 233 68)
               |> move (0,43.75)
               ,rect 10 6.25
               |> filled (rgb 22 46 56)
               |> move (0,37.5)
               ,rect 10 6.25
               |> filled (rgb 50 115 83)
               |> move (0,31.25)
               ,rect 10 6.25
               |> filled (rgb 40 154 242)
               |> move (0,25)
               ,rect 10 6.25
               |> filled (rgb 29 77 175)
               |> move (0,18.75)
               ,rect 10 6.25
               |> filled (rgb 247 126 159)
               |> move (0,12.5)
               ,rect 10 8
               |> filled (rgb 244 72 58)
               |> move (0,8)
               ,rect 6 6.25
               |> filled (rgb 221 116 51)
               |> move (9,50)
               ,rect 6 6.25
               |> filled (rgb 252 233 68)
               |> move (9,43.75)
               ,rect 6 6.25
               |> filled (rgb 22 46 56)
               |> move (9,37.5)
               ,rect 6 6.25
               |> filled (rgb 50 115 83)
               |> move (9,31.25)
               ,rect 12 6.25
               |> filled (rgb 40 154 242)
               |> move (12,25)
               ,rect 15 6.25
               |> filled (rgb 29 77 175)
               |> move (15,18.75)
               ,rect 18 6.25
               |> filled (rgb 247 126 159)
               |> move (18,12.5)
               ,rect 25 8
               |> filled (rgb 244 72 58)
               |> move (20.5,8)
               ,curve (0,0) [Pull (40,0) (0,0)
               ,Pull (0,0) (50,50)
               ,Pull (0,0) (50,-50)]
               |> filled white
               |> scale 0.5
               |> move (5.25,3)
               |> rotate (degrees 45)
               ,curve (0,0) [Pull (12,0) (15,-10) ]
               |> filled white
               |> move (41.25,7.5)
               |> rotate (degrees 178)
               |>scale 2
               ,curve (0,0) [Pull (16.5,0) (15,-25) ]
               |> filled white
               |> move (26.5,15)
               |> rotate (degrees 178)
               |>scale 1
               ,rect 6 6.25
               |> filled (rgb 221 116 51)
               |> move (-9,50)
               ,rect 6 6.25
               |> filled (rgb 252 233 68)
               |> move (-9,43.75)
               ,rect 6 6.25
               |> filled (rgb 22 46 56)
               |> move (-9,37.5)
               ,rect 6 6.25
               |> filled (rgb 50 115 83)
               |> move (-9,31.25)
               ,rect 12 6.25
               |> filled (rgb 40 154 242)
               |> move (-12,25)
               ,rect 15 6.25
               |> filled (rgb 29 77 175)
               |> move (-15,18.75)
               ,rect 18 6.25
               |> filled (rgb 247 126 159)
               |> move (-18,12.5)
               ,rect 25 8
               |> filled (rgb 244 72 58)
               |> move (-20.5,8)
               ,curve (0,0) [Pull (-40,0) (0,0)
               ,Pull (0,0) (-50,50)
               ,Pull (0,0) (-50,-50)]
               |> filled white
               |> scale 0.5
               |> move (-5.25,3)
               |> rotate (degrees -45)
               ,curve (0,0) [Pull (12,0) (15,10) ]
               |> filled white
               |> move (-41.25,7.5)
               |> rotate (degrees 2)
               |>scale 2
               ,curve (0,0) [Pull (16.5,0) (15,25) ]
               |> filled white
               |> move (-26.5,15)
               |> rotate (degrees 2)
               |>scale 1
        ]

mario = group [ circle 60
                |> outlined (solid 5) (rgb 255 0 0)
                ,rectangle 15 15
                |> filled (rgb 255 0 0)
                |> scaleY 4.45
                |> rotate (degrees -20)
                |> move (-32,7)
                ,rectangle 15 15
                |> filled (rgb 255 0 0)
                |> scaleY 4.45
                |> rotate (degrees 20)
                |> move (32,7)
                ,rectangle 15 15
                |> filled (rgb 255 0 0)
                |> scaleY 2.75
                |> rotate (degrees 45)
                |> move (-13.8,15.1)
                ,rectangle 15 15
                |> filled (rgb 255 0 0)
                |> scaleY 2.75
                |> rotate (degrees -45)
                |> move (13.8,15.1)
                ,rectangle 10 10
                |> filled white
                |> scaleY 2.75
                |> rotate (degrees -45)
                |> move (16,34.9)
                 ,rectangle 10 10
                |> filled white
                |> scaleY 2.75
                |> rotate (degrees 45)
                |> move (-16,34.9)
                ,square 10
                |> filled (rgb 255 0 0)
                |> rotate (degrees 45)
                |> move (0,-2.3) ]
                
kirby = group [ circle 40
                |> filled (rgb 255 203 207)
                ,oval 19 19.5
                |> filled (rgb 255 171 174)
                |> scaleY 1.75
                |> scale 0.4
                |> move (21.5, 0)
                |> rotate (degrees 90)
                ,oval 19 19.5
                |> filled (rgb 255 171 174)
                |> scaleY 1.75
                |> scale 0.4
                |> move (-21.5, 0)
                |> rotate (degrees 90)
                ,circle 40
                |> outlined (solid 4)(rgb 104 31 42)
                ,oval 18 22
                |> filled (rgb 104 31 42)
                |> scaleY 1.75
                |> scale 0.6
                |> move (-13, 10)
                ,oval 18 22
                |> filled (rgb 104 31 42)
                |> scaleY 1.75
                |> scale 0.6
                |> move (13, 10)
                ,oval 18 20
                |> filled white
                |> scaleY 1.75
                |> scale 0.3
                |> move (-13, 14)
                ,oval 18 20
                |> filled white
                |> scaleY 1.75
                |> scale 0.3
                |> move (13, 14)
                ,oval 18 20
                |> filled (rgb 255 112 116)
                |> scaleY 1.75
                |> scale 0.3
                |> move (21.5, 0)
                |> rotate (degrees 90)
                ,oval 18 20
                |> filled (rgb 255 112 116)
                |> scaleY 1.75
                |> scale 0.3
                |> move (-21.5, 0)
                |> rotate (degrees 90)
                ,wedge 5 0.5
                |> outlined (solid 1.25) (rgb 104 31 42)
                |> rotate (degrees -90)
                |> move (0,-5)
                , rectangle 4 12
                |> filled (rgb 255 203 207)
                |> rotate (degrees 90)
                |> move (0,-5)
                , circle 1.25
                |> filled (rgb 104 31 42)
                |> move (-4.5,-6.5)
                , circle 1.25
                |> filled (rgb 104 31 42)
                |> move (4.5,-6.5) ]
                
pacman c t = group [wedge 50 (0.75 + abs(sin(t/5)/4))
                     |> filled c ]
                  
 
powerbutton = group [circle 50
              |> filled black
              , circle 40
              |> filled (rgb 21 243 5)
              , circle 30
              |> filled black
              ,triangle 26
              |> filled black
              |> move (0,28)
              |> rotate (degrees 150)
              ,roundedRect 10 40 5
              |> filled (rgb 21 243 5)
              |> move (0,25)
              ]
powerbutton1 = group [circle 50
              |> filled black
              , circle 40
              |> filled (rgb 255 255 0)
              , circle 30
              |> filled black
              ,triangle 26
              |> filled black
              |> move (0,28)
              |> rotate (degrees 150)
              ,roundedRect 10 40 5
              |> filled (rgb 255 255 0)
              |> move (0,25)
              ]
powerbutton2 = group [circle 50
              |> filled black
              , circle 40
              |> filled (rgb 255 14 0)
              , circle 30
              |> filled black
              ,triangle 26
              |> filled black
              |> move (0,28)
              |> rotate (degrees 150)
              ,roundedRect 10 40 5
              |> filled (rgb 255 14 0)
              |> move (0,25)
              ]
instructionsbutton1 = group [ roundedRect 400 75 30 
    |> filled darkGray
    |> move (0, -250)
    ,text "Instructions"
    |> filled white 
    |> move (-140, -270)
    |>scale 5]
pedastal = group [ 
             polygon [ (10,0),(7.5,5),(2.5,5),(0,0) ]
              |> filled grey 
             ,triangle 1
              |> filled white
              |> move (4.15,1.75)
              |> rotate (degrees -30)
            ,triangle 1
              |> filled white
              |> move (5,3.3)
              |> rotate (degrees -30)
            ,triangle 1
              |> filled white
              |> move (5.85,1.75)
              |> rotate (degrees -30)
             ]
halflife = group [ circle 40
                      |> outlined (solid 8) orange
                      |> move (-11,5)
                    ,rectangle 10 15
                      |> filled orange
                      |> rotate (degrees 90)
                      |> move (-20, 25)
                      |> scaleX 0.75
                      ,rectangle 10 15
                      |> filled orange
                      |> rotate (degrees 205)
                      |> move (-6.45, 6.75)
                      |> scaleX 0.75
                      |> scaleY 3
                      ,rectangle 10 15
                      |> filled orange
                      |> rotate (degrees 115)
                      |> move (6.05, -11)
                      |> scaleX 0.75
                      ,rectangle 10 15
                      |> filled orange
                      |> rotate (degrees 150)
                      |> move (-20, 0)
                      |> scaleX 0.75
                      |> scaleY 2.5
                      ,rectangle 10 15
                      |> filled white
                      |> rotate (degrees 90)
                      |> move (-25.5, -18.5) ]

pokeball = group [ wedge 40 0.5
 |> filled red
 |> rotate (degrees 90)            
  ,wedge 40 0.5
 |> filled white
 |> rotate (degrees -90)     
  ,circle 40 
 |> outlined (solid 1.5) black          
  ,rectangle 2.5 30
 |> filled black
 |> rotate (degrees 90)

 |> move (24,0)
  ,rectangle 2.5 30
 |> filled black
 |> rotate (degrees 90)

 |> move (-24,0)
  ,circle 8.5
 |> outlined (solid 2.25) black
  ,circle 8
 |> filled white
  ,circle 5
 |> outlined (solid 1) black
 ]
 
mastersword = group [ polygon [(0,20),(0,0),(5,0)]
              |> filled (rgb 220 229 249)
              |> move (0,40)
            ,rect 5 13
              |> filled (rgb 183 197 232)
              |> move (0,-22)
            ,oval 16 8
              |> filled (rgb 183 197 232)
              |> move (0,-13)
            ,curve (0,0) [Pull (-5,0) (-11,14)]
              |> filled (rgb 220 229 249)
              |> move (8.25,-12.5)
              |> rotate (degrees 10)
            ,curve (0,0) [Pull (-5,0) (-11,-14)]
              |> filled (rgb 220 229 249)
              |> move (-8.25,-12.5)
              |> rotate (degrees -190)
            ,curve (0,0) [Pull (-10,0) (-19,20)]
              |> filled (rgb 220 229 249)
              |> move (-8.25,-13.25)
              |> rotate (degrees 170)
            ,curve (0,0) [Pull (-10,0) (-19,-20)]
              |> filled (rgb 220 229 249)
              |> move (8.25,-13.25)
              |> rotate (degrees 10)
            ,polygon [(15,20),(10,0),(15,0)]
              |> filled (rgb 220 229 249)
              |> move (-15,40)
            ,polygon [(10,15),(7.5,0),(10,0)]
              |> filled (rgb 183 197 232)
              |> move (-10,40)
            ,polygon [(0,15),(0,0),(2.5,0)]
              |> filled (rgb 183 197 232)
              |> move (0,40)
            ,rect 10 50 
              |> filled (rgb 220 229 249)
              |> move (0,15)
            ,rect 5 50 
              |> filled (rgb 183 197 232)
              |> move (0,15)
            ,roundedRect 10 12 1
              |> filled (rgb 220 229 249)
              |> move (0,-22)
            ,rect 5 13
              |> filled (rgb 183 197 232)
              |> move (0,-22)
            ,triangle 1
              |> filled (rgb 220 229 249)
              |> move (0,-12)
              |> rotate (degrees -30)
            ,triangle 1
              |> filled (rgb 220 229 249)
              |> move (1,-14)
              |> rotate (degrees -30)
            ,triangle 1
              |> filled (rgb 220 229 249)
              |> move (-1,-14)
              |> rotate (degrees -30)
            ,rect 10 10
              |> filled (rgb 45 33 127)
              |> move (0,-33.5)
            ,rect 2 2
              |> filled (rgb 254 235 217)
              |> move (-4,-28)
            ,rect 2 2
              |> filled (rgb 254 235 217)
              |> move (4,-28)
            ,roundedRect 3 5 1
              |> filled (rgb 183 197 232)
              |> move (0,-28)
            ,rect 2 2
              |> filled (rgb 254 235 217)
              |> move (0,-32.5)
            ,triangle 1
              |> filled (rgb 254 235 217)
              |> move (0,-31)
              |> rotate (degrees -30)
            ,roundedRect 4 13 1
              |> filled (rgb 45 33 127)
              |> move (0,-43)
            ,oval 6 4
              |> filled (rgb 45 33 127)
              |> move (0,-49)
            ,oval 3 4
              |> filled (rgb 45 33 127)
              |> move (0,-50)
            ,oval 8 5
              |> filled (rgb 45 33 127)
              |> move (0,-38)
            ,polygon [(20,9),(10,0),(20,0)]
              |> filled (rgb 45 33 127)
              |> move (-23,-38)
            ,polygon [(-20,9),(-10,0),(-20,0)]
              |> filled (rgb 45 33 127)
              |> move (23,-38)
            ,curve (0,0) [Pull (-10,0) (-10,5)]
              |> filled (rgb 45 33 127)
              |> move (4.5,-30.75)
              |> rotate (degrees 165)
            ,curve (0,0) [Pull (-10,0) (-12,-5)]
              |> filled (rgb 45 33 127)
              |> move (8,-37)
              |> rotate (degrees 210)
            ,curve (0,0) [Pull (-10,0) (-12,-5)]
              |> filled (rgb 45 33 127)
              |> move (5.5,-37)
              |> rotate (degrees 210)
            ,curve (0,0) [Pull (-10,0) (-12,-5)]
              |> filled (rgb 45 33 127)
              |> move (3,-37)
              |> rotate (degrees 210)
            ,curve (0,0) [Pull (-10,0) (-12,5)]
              |> filled (rgb 45 33 127)
              |> move (-8,-37)
              |> rotate (degrees -30)
            ,curve (0,0) [Pull (-10,0) (-10,-5)]
              |> filled (rgb 45 33 127)
              |> move (-4.5,-30.75)
              |> rotate (degrees 15)
            ,curve (0,0) [Pull (-10,0) (-12,5)]
              |> filled (rgb 45 33 127)
              |> move (-5.5,-37)
              |> rotate (degrees -30)
            ,curve (0,0) [Pull (-10,0) (-12,5)]
              |> filled (rgb 45 33 127)
              |> move (-3,-37)
              |> rotate (degrees -30)
            ,rect 2 2
              |> filled (rgb 254 235 217)
              |> move (0,-34)
              |> rotate (degrees 45)
           ]

                

        


--- UPDATE ---

update msg model = case msg of
                        Tick t _ -> { model | state = if model.state == InGame && model.levels == []
                                                            then EndOfGame else if model.state == InGame && model.timelimit <= 0 
                                                            then Failure
                                                         
                                                            else model.state
                                    ,         time = model.time + 1
                                    ,         timelimit = if model.state == InGame then model.timelimit - 0.04 else model.timelimit 
                                   
                                    }
                        StartGame -> { model | state = InGame}
                        GoInstructions -> { model | state = Instructions}
                        SubmitAnswer ans1 ans2 -> if ans1 == ans2
                                                    then nextLevel model
                                                    else wrongAnswer model
                        Reset -> init

nextLevel model = {model | levels = Maybe.withDefault [] (List.tail model.levels) , time = 0 ,timelimit = 20,score = toFloat (round(model.score + model.timelimit))}

wrongAnswer model = case model.chances of
                        0 -> {model | state = Failure}
                        _ -> {model | chances = model.chances - 1}

