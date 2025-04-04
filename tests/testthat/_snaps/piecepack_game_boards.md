# rectangular boards works as expected

    Code
      df <- piecepack_rectangular_board(nrows = 8, ncols = 8)
      cpiece(df)
    Output
       ┌─┰─┬─┰─┬─┰─┬─┰─┐
       │ ┃ │ ┃ │ ┃ │ ┃ │
       ┝━╋━┿━╋━┿━╋━┿━╋━┥
       │ ┃ │ ┃ │ ┃ │ ┃ │
       ├─╂─┼─╂─┼─╂─┼─╂─┤
       │ ┃ │ ┃ │ ┃5⃝│ ┃ │
       ┝━╋━┿━╋━┿━╋━┿━╋━┥
       │ ┃ │ ┃ │4⃝┃ │ ┃ │
       ├─╂─┼─╂─┼─╂─┼─╂─┤
       │ ┃ │ ┃3⃝│ ┃ │ ┃ │
       ┝━╋━┿━╋━┿━╋━┿━╋━┥
       │ ┃ │2⃝┃ │ ┃ │ ┃ │
       ├─╂─┼─╂─┼─╂─┼─╂─┤
       │ ┃a⃝│ ┃ │ ┃ │ ┃ │
       ┝━╋━┿━╋━┿━╋━┿━╋━┥
       │n⃝┃ │ ┃ │ ┃ │ ┃ │
       └─┸─┴─┸─┴─┸─┴─┸─┘
                        

---

    Code
      df <- piecepack_rectangular_board(nrows = 10, ncols = 10)
      cpiece(df)
    Output
        ┌─┰─┬─┰─┐ ┌─┰─┬─┰─┐
        │ ┃ │ ┃ │ │ ┃ │ ┃ │
        ┝━╋━┿━╋━┥ ┝━╋━┿━╋━┥
        │ ┃ │ ┃ │ │ ┃ │ ┃ │
        ├─╂─┼─╂─┤ ├─╂─┼─╂─┤
        │ ┃ │ ┃ │ │ ┃ │ ┃ │
        ┝━╋━┿━╋━┥ ┝━╋━┿━╋━┥
        │ ┃ │ ┃ │ │ ┃ │ ┃ │
        └─┸─┴─┸─┘ 5⃝─┸─┴─┸─┘
                           
        ┌─┰─┬─┰─4⃝ ┌─┰─┬─┰─┐
        │ ┃ │ ┃ │ │ ┃ │ ┃ │
        ┝━╋━┿━3⃝━┥ ┝━╋━┿━╋━┥
        │ ┃ │ ┃ │ │ ┃ │ ┃ │
        ├─╂─2⃝─╂─┤ ├─╂─┼─╂─┤
        │ ┃ │ ┃ │ │ ┃ │ ┃ │
        ┝━a⃝━┿━╋━┥ ┝━╋━┿━╋━┥
        │ ┃ │ ┃ │ │ ┃ │ ┃ │
        n⃝─┸─┴─┸─┘ └─┸─┴─┸─┘
                           
                           

---

    Code
      df <- piecepack_rectangular_board(nrows = 8, ncols = 4)
      cpiece(df)
    Output
       ┌─┰─┬─┰─┐    
       │ ┃ │ ┃ │    
       ┝━╋━┿━╋━┥    
       │ ┃ │ ┃ │    
       ├─╂─┼─╂─┤    
       │ ┃ │ ┃ │  5⃝ 
       ┝━╋━┿━╋━┥    
       │ ┃ │ ┃ │4⃝   
       ├─╂─┼─╂─┤    
       │ ┃ │ ┃3⃝│    
       ┝━╋━┿━╋━┥    
       │ ┃ │2⃝┃ │    
       ├─╂─┼─╂─┤    
       │ ┃a⃝│ ┃ │    
       ┝━╋━┿━╋━┥    
       │n⃝┃ │ ┃ │    
       └─┸─┴─┸─┘    
                    

---

    Code
      df <- piecepack_rectangular_board(nrows = 6, ncols = 2)
      cpiece(df)
    Output
       ┌─┰─┐        
       │ ┃ │      5⃝ 
       ┝━╋━┥        
       │ ┃ │    4⃝   
       ├─╂─┤        
       │ ┃ │  3⃝     
       ┝━╋━┥        
       │ ┃ │2⃝       
       ├─╂─┤        
       │ ┃a⃝│        
       ┝━╋━┥        
       │n⃝┃ │        
       └─┸─┘        
                    

---

    Code
      df <- piecepack_rectangular_board(nrows = 5, ncols = 3)
      cpiece(df)
    Output
                    
                  5⃝ 
                    
        ┌─┰─┐   4⃝   
        │ ┃ │       
        ┝━╋━┥ 3⃝     
        │ ┃ │       
        ├─╂─2⃝       
        │ ┃ │       
        ┝━a⃝━┥       
        │ ┃ │       
        n⃝─┸─┘       
                    
                    

---

    Code
      df <- piecepack_rectangular_board(nrows = 5, ncols = 7)
      cpiece(df)
    Output
                     
                  5⃝  
                     
        ┌─┰─┬─┰─4⃝─┰─┐
        │ ┃ │ ┃ │ ┃ │
        ┝━╋━┿━3⃝━┿━╋━┥
        │ ┃ │ ┃ │ ┃ │
        ├─╂─2⃝─╂─┼─╂─┤
        │ ┃ │ ┃ │ ┃ │
        ┝━a⃝━┿━╋━┿━╋━┥
        │ ┃ │ ┃ │ ┃ │
        n⃝─┸─┴─┸─┴─┸─┘
                     
                     

---

    Code
      df <- piecepack_rectangular_board(nrows = 5, ncols = 6)
      cpiece(df)
    Output
                    
                  5⃝ 
                    
        ┌─┰─┐ ┌─4⃝─┐ 
        │ ┃ │ │ ┃ │ 
        ┝━╋━┥ 3⃝━╋━┥ 
        │ ┃ │ │ ┃ │ 
        ├─╂─2⃝ ├─╂─┤ 
        │ ┃ │ │ ┃ │ 
        ┝━a⃝━┥ ┝━╋━┥ 
        │ ┃ │ │ ┃ │ 
        n⃝─┸─┘ └─┸─┘ 
                    
                    

---

    Code
      df <- piecepack_rectangular_board(nrows = 8, ncols = 8, max_tiles = 12)
      cpiece(df)
    Output
        ┌─┰─┐ ┌─┰─┬─┰─┐
        │ ┃ │ │ ┃ │ ┃ │
        ┝━╋━┥ ┝━╋━┿━╋━┥
        │ ┃ │ │ ┃ │ ┃ │
        ├─╂─┤ ├─╂─5⃝─╂─┤
        │ ┃ │ │ ┃ │ ┃ │
        ┝━╋━┥ ┝━4⃝━┿━╋━┥
        │ ┃ │ │ ┃ │ ┃ │
        └─┸─┘ 3⃝─┸─┴─┸─┘
                       
        ┌─┰─2⃝ ┌─┰─┬─┰─┐
        │ ┃ │ │ ┃ │ ┃ │
        ┝━a⃝━┥ ┝━╋━┿━╋━┥
        │ ┃ │ │ ┃ │ ┃ │
        n⃝─┸─┘ └─┸─┴─┸─┘
                       
                       

