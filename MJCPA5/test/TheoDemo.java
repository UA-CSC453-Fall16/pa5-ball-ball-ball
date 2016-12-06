import meggy.Meggy;

class PA5demo{
	//This class only creates an instance of the PA5Execution class
	//for all of the computation.
	public static void main(String[] args){
        new PA5Execution().displayZeepAndStartAdventure(); //Show the main character of our adventure

	}
}

class PA5Execution{

    int level;
    Meggy.Color red;
    Meggy.Color white;
    Meggy.Tone dun;
    Meggy.Tone dundun;
    Meggy.Tone dundundun;
    Meggy.Color[] colors;
    int curCol;

    int unused;
    boolean unBool;

	public void displayZeepAndStartAdventure(){
        boolean waitToStart;
        int row;
        int col;
        byte val;
        Screen screen;
        Sprites sp;

        Zeep zeep;

        zeep = new Zeep();
        row = (byte) 0;
        zeep.initZeep();

        screen = new Screen();
        sp = new Sprites();

        red = Meggy.Color.RED;
        white = Meggy.Color.WHITE;
        colors = new Meggy.Color[4];
        colors[0] = red;
        colors[1] = white;
        colors[2] = Meggy.Color.GREEN; //Used to alternate between blue and green for
        colors[3] = Meggy.Color.BLUE;  //the target object

        //Not going to lie, these three lines are used to completely use the grammar
        //My program only needs 46/49 of the grammar.
        unused = -1;
        //unBool = this == this;
        unused = (byte) 4 * (byte) 2; 

        dun       = Meggy.Tone.G3;
        dundun    = Meggy.Tone.E3;
        dundundun = Meggy.Tone.Cs3; //For the dun dun dunnnn sound effect 

        //Draw Zeep for the start screen!
        while(row < 8){
            col = (byte) 0;
            while(col < 8){
                val = zeep.access2D(col, row);
                Meggy.setPixel((byte)row, (byte)col, white);
                if(val == 1){
                    Meggy.setPixel((byte)row, (byte)col, red);
                }
                col = col + 1;
            }
            row = row + 1;
        }

        //When the start screen is displayed, wait until a or b is pressed to start the game
        waitToStart = true;
        while(waitToStart){
            if(Meggy.checkButton(Meggy.Button.A))
                waitToStart = false;
            if(Meggy.checkButton(Meggy.Button.B))
                waitToStart = false;
        }

        screen.clearScreen();
        level = 1;
        screen.dundundun(level, dun, dundun, dundundun);

        curCol = colors.length - 1;
        screen.displayZeep(sp.getZX(), sp.getZY());
        Meggy.delay(100);

        //Set initial level conditions
        if(level == 1 && Meggy.getPixel((byte) 2, (byte) 2) == Meggy.Color.WHITE){
                sp.setLevel(level);
        }

        unBool = !false;

        while(unBool){
            //reset level
            if(Meggy.checkButton(Meggy.Button.B) && Meggy.checkButton(Meggy.Button.A)){
                screen.dundundun(level, dun, dundun, dundundun);
                screen.clearScreen();
                sp.setLevel(level);
            }

            //Check for movement of character 
            if(Meggy.checkButton(Meggy.Button.Up) && sp.getZY() < 6)
                sp.up();
            if(Meggy.checkButton(Meggy.Button.Down) && 0 < sp.getZY())
                sp.down();
            if(Meggy.checkButton(Meggy.Button.Left) && 0 < sp.getZX())
                sp.left();
            if(Meggy.checkButton(Meggy.Button.Right) && sp.getZX() < 6)
                sp.right();

            //Clear the screen except the location of the character
            screen.clearScreenExcept(sp.getZX(), sp.getZY());
            screen.displayZeep(sp.getZX(), sp.getZY());

            //Allow target object to flicker blue and green
            if(curCol == 3)
                curCol = 2;
            else
                curCol = 3;

            //Check to see if the game has been won. If it has, end the animation loop
            //And display end game screen
            if(sp.win()){
                level = level + 1;
                if(8 < level){
                    unBool = false;
                }
                screen.dundundun(level, dun, dundun, dundundun);
                sp.setLevel(level);
                screen.clearScreen();
            }

            //Draw the target and the goal on the screen
            screen.displayTarget(sp.getTX(), sp.getTY(), colors[curCol]);
            screen.displayGoal(sp.getGX(), sp.getGY(), Meggy.Color.ORANGE);

            //Gotta get that 30 fps gameplay, -> delay 33 ms
            Meggy.delay(33);
            
        }

        //Congratulations! You won Zeep!
        screen.dundundun(8, dun, dundun, dundundun);
        screen.clearScreen();
        screen.displayWin();
    }
}

class Sprites{
    int targetx; //Target is the movable object
    int targety;
    int zeepx;   //Zeep is the moveable character
    int zeepy;
    int goalx;   //goal is where the object needs to move to
    int goaly;

    //set the x / y location of the target object
    public void setTarget(int x, int y){
        targetx = x;
        targety = y;
    }

    public int getTX(){
        return targetx;
    }

    public int getTY(){
        return targety;
    }

    //set the x / y location of the goal object
    public void setGoal(int x, int y){
        goalx = x;
        goaly = y;
    }

    public int getGX(){
        return goalx;
    }

    public int getGY(){
        return goaly;
    }

    //set the x / y location of the player object
    public void setZeep(int x, int y){
        zeepx = x;
        zeepy = y;
    }

    public int getZX(){
        return zeepx;
    }

    public int getZY(){
        return zeepy;
    }

    //check to see if the level has been won (target and goal locations the same)
    public boolean win(){
        boolean retVal;
        retVal = false;
        if(targetx == goalx && targety == goaly)
            retVal = true;
        return retVal;
    }


    /*
    * Movement methods, move player object and target object in the 
    * case that moving the player will overlap the target
    */
    public void right(){
        if(targetx == zeepx + 2 && targety < zeepy + 2 && zeepy - 1 < targety)
            targetx = targetx + 1;
        zeepx = zeepx + 1;
    }

    public void left(){
        if(targetx == zeepx - 1 && targety < zeepy + 2 && zeepy - 1 < targety)
            targetx = targetx - 1;
        zeepx = zeepx - 1;
    }

    public void up(){
        if(targety == zeepy + 2 && targetx < zeepx + 2 && zeepx - 1 < targetx)
            targety = targety + 1;
        zeepy = zeepy + 1;
    }

    public void down(){
    if(targety == zeepy - 1 && targetx < zeepx + 2 && zeepx - 1 < targetx)
            targety = targety - 1;
        zeepy = zeepy - 1;
    }

    //Hard coded level presets: starting locations for the target goal and player
    public void setLevel(int level){
        if(level == 1){
            zeepx = 3;
            zeepy = 3;
            goalx = 7;
            goaly = 5;
            targetx = 2;
            targety = 2;
        }
        if(level == 2){
            zeepx = 0;
            zeepy = 0;
            goalx = 2;
            goaly = 2;
            targetx = 4;
            targety = 5;
        }
        if(level == 3){
            zeepx = 6;
            zeepy = 6;
            goalx = 0;
            goaly = 0;
            targetx = 3;
            targety = 3;
        }
        if(level == 4){
            zeepx = 6;
            zeepy = 0;
            goalx = 6;
            goaly = 2;
            targetx = 2;
            targety = 4;
        }
        if(level == 5){
            zeepx = 0;
            zeepy = 0;
            goalx = 3;
            goaly = 1;
            targetx = 2;
            targety = 1;
        }
        if(level == 6){
            zeepx = 6;
            zeepy = 5;
            goalx = 0;
            goaly = 5;
            targetx = 0;
            targety = 2;
        }
        if(level == 7){
            zeepx = 6;
            zeepy = 3;
            goalx = 4;
            goaly = 3;
            targetx = 5;
            targety = 2;
        }
        if(level == 8){
            zeepx = 2;
            zeepy = 5;
            goalx = 6;
            goaly = 5;
            targetx = 5;
            targety = 5;
        }

    }

}

class Screen{
    //White out the screen
    public void clearScreen(){
        int i;
        int j;
        i = (byte) 0;
        while(i < 8){
            j = (byte) 0;
            while(j < 8){
                Meggy.setPixel((byte) j, (byte) i, Meggy.Color.WHITE);
                j = j + 1;
            }
            i = i + 1;
        }
    }

    //white out the screen except the player's location (prevents flickering)
    public void clearScreenExcept(int x, int y){
        int i;
        int j;
        i = (byte) 0;
        while(i < 8){
            j = (byte) 0;
            while(j < 8){
                if(j < x)
                    Meggy.setPixel((byte) j, (byte) i, Meggy.Color.WHITE);
                if(i < y)
                    Meggy.setPixel((byte) j, (byte) i, Meggy.Color.WHITE);
                if(y + 1 < i )
                    Meggy.setPixel((byte) j, (byte) i, Meggy.Color.WHITE);
                if(x + 1 < j)
                    Meggy.setPixel((byte) j, (byte) i, Meggy.Color.WHITE);
                j = j + 1;
            }
            i = i + 1;
        }
    }

    //Draw player character on screen
    public void displayZeep(int x, int y){
        Meggy.setPixel((byte) x, (byte) y, Meggy.Color.RED);
        Meggy.setPixel((byte) x, (byte) (y + 1), Meggy.Color.RED);
        Meggy.setPixel((byte) (x + 1), (byte) y, Meggy.Color.RED);
        Meggy.setPixel((byte) (x + 1), (byte) (y + 1), Meggy.Color.RED);
    }

    //Draw target object on screen
    public void displayTarget(int x, int y, Meggy.Color col){
        Meggy.setPixel((byte) x, (byte) y, col);
    }

    //Draw goal object on screen
    public void displayGoal(int x, int y, Meggy.Color col){
        Meggy.setPixel((byte) x, (byte) y, col);
    }

    //Play dun dun dunnnn sound
    public void dundundun(int level, Meggy.Tone one, Meggy.Tone two, Meggy.Tone three){
        int bits;
        bits = 1;
        if(level == 1)
            bits = 1;
        if(level == 2)
            bits = 3;
        if(level == 3)
            bits = 7;
        if(level == 4)
            bits = 15;
        if(level == 5)
            bits = 31;
        if(level == 6)
            bits = 63;
        if(level == 7)
            bits = 127;
        if(level == 8)
            bits = 255;

        Meggy.toneStart(one, 200);
        Meggy.delay(200);
        Meggy.toneStart(two, 300);
        Meggy.delay(300);
        Meggy.toneStart(three, 1000);
        Meggy.delay(500);

        Meggy.setAuxLEDs (bits);
        Meggy.toneStart(Meggy.Tone.B3, 100);
        Meggy.delay(100);
        Meggy.setAuxLEDs (0);
        Meggy.toneStart(Meggy.Tone.B3, 100);
        Meggy.delay(100);
        Meggy.setAuxLEDs (bits);
        Meggy.toneStart(Meggy.Tone.B3, 100);
        Meggy.delay(100);
        Meggy.setAuxLEDs (0);
        Meggy.toneStart(Meggy.Tone.B3, 100);
        Meggy.delay(100);
        Meggy.setAuxLEDs (bits);
        Meggy.toneStart(Meggy.Tone.B3, 100);
        Meggy.delay(100);

        Meggy.toneStart(Meggy.Tone.Gs3, 400);
    }

    //Final "ZEEP" screen
    public void displayWin(){
        Meggy.toneStart(Meggy.Tone.G3, 200);
        Meggy.delay(200);
        Meggy.toneStart(Meggy.Tone.Fs3, 200);
        Meggy.delay(200);
        Meggy.toneStart(Meggy.Tone.Ds3, 200);
        Meggy.delay(200);
        Meggy.toneStart(Meggy.Tone.B3, 200);
        Meggy.delay(200);
        Meggy.toneStart(Meggy.Tone.Gs3, 25);
        Meggy.delay(25);
        Meggy.toneStart(Meggy.Tone.E3, 25);
        Meggy.delay(25);
        Meggy.toneStart(Meggy.Tone.Gs3, 25);
        Meggy.delay(25);
        Meggy.toneStart(Meggy.Tone.E3, 25);
        Meggy.delay(25);
        Meggy.toneStart(Meggy.Tone.Gs3, 25);
        Meggy.delay(25);
        Meggy.toneStart(Meggy.Tone.E3, 25);
        Meggy.delay(25);
        Meggy.toneStart(Meggy.Tone.Gs3, 25);
        Meggy.delay(25);
        Meggy.toneStart(Meggy.Tone.C3, 200);
        Meggy.delay(200);

        Meggy.setPixel((byte) 0, (byte) 0, Meggy.Color.RED);
        Meggy.setPixel((byte) 0, (byte) 1, Meggy.Color.RED);
        Meggy.setPixel((byte) 0, (byte) 2, Meggy.Color.RED);
        Meggy.setPixel((byte) 0, (byte) 4, Meggy.Color.RED);
        Meggy.setPixel((byte) 0, (byte) 7, Meggy.Color.RED);
        Meggy.setPixel((byte) 1, (byte) 0, Meggy.Color.RED);
        Meggy.setPixel((byte) 1, (byte) 2, Meggy.Color.RED);
        Meggy.setPixel((byte) 1, (byte) 3, Meggy.Color.RED);
        Meggy.setPixel((byte) 1, (byte) 4, Meggy.Color.RED);
        Meggy.setPixel((byte) 1, (byte) 5, Meggy.Color.RED);
        Meggy.setPixel((byte) 1, (byte) 7, Meggy.Color.RED);
        Meggy.setPixel((byte) 2, (byte) 0, Meggy.Color.RED);
        Meggy.setPixel((byte) 2, (byte) 2, Meggy.Color.RED);
        Meggy.setPixel((byte) 2, (byte) 4, Meggy.Color.RED);
        Meggy.setPixel((byte) 2, (byte) 6, Meggy.Color.RED);
        Meggy.setPixel((byte) 2, (byte) 7, Meggy.Color.RED);
        Meggy.setPixel((byte) 3, (byte) 4, Meggy.Color.RED);
        Meggy.setPixel((byte) 3, (byte) 7, Meggy.Color.RED);
        Meggy.setPixel((byte) 4, (byte) 0, Meggy.Color.RED);
        Meggy.setPixel((byte) 4, (byte) 1, Meggy.Color.RED);
        Meggy.setPixel((byte) 4, (byte) 2, Meggy.Color.RED);
        Meggy.setPixel((byte) 4, (byte) 3, Meggy.Color.RED);
        Meggy.setPixel((byte) 5, (byte) 1, Meggy.Color.RED);
        Meggy.setPixel((byte) 5, (byte) 3, Meggy.Color.RED);
        Meggy.setPixel((byte) 5, (byte) 4, Meggy.Color.RED);
        Meggy.setPixel((byte) 5, (byte) 5, Meggy.Color.RED);
        Meggy.setPixel((byte) 5, (byte) 6, Meggy.Color.RED);
        Meggy.setPixel((byte) 6, (byte) 1, Meggy.Color.RED);
        Meggy.setPixel((byte) 6, (byte) 2, Meggy.Color.RED);
        Meggy.setPixel((byte) 6, (byte) 3, Meggy.Color.RED);
        Meggy.setPixel((byte) 6, (byte) 4, Meggy.Color.RED);
        Meggy.setPixel((byte) 6, (byte) 6, Meggy.Color.RED);
        Meggy.setPixel((byte) 6, (byte) 7, Meggy.Color.RED);
        Meggy.setPixel((byte) 7, (byte) 4, Meggy.Color.RED);
        Meggy.setPixel((byte) 7, (byte) 6, Meggy.Color.RED);
    }
}


//Start Screen information
class Zeep{

    int[] zeep0;
    int[] zeep1;
    int[] zeep2;
    int[] zeep3;
    int[] zeep4;
    int[] zeep5;
    int[] zeep6;
    int[] zeep7;

    public void initZeep(){
        zeep0 = new int[8];
        zeep1 = new int[8];
        zeep2 = new int[8];
        zeep3 = new int[8];
        zeep4 = new int[8];
        zeep5 = new int[8];
        zeep6 = new int[8];
        zeep7 = new int[8];
        
        zeep0 [0] = 0;
        zeep0 [1] = 0;
        zeep0 [2] = 1;
        zeep0 [3] = 0;
        zeep0 [4] = 0;
        zeep0 [5] = 1;
        zeep0 [6] = 0;
        zeep0 [7] = 0;

        zeep1 [0] = 0;
        zeep1 [1] = 1;
        zeep1 [2] = 0;
        zeep1 [3] = 1;
        zeep1 [4] = 1;
        zeep1 [5] = 0;
        zeep1 [6] = 1;
        zeep1 [7] = 0;

        zeep2 [0] = 0;
        zeep2 [1] = 0;
        zeep2 [2] = 1;
        zeep2 [3] = 0;
        zeep2 [4] = 0;
        zeep2 [5] = 1;
        zeep2 [6] = 0;
        zeep2 [7] = 0;

        zeep3 [0] = 0;
        zeep3 [1] = 0;
        zeep3 [2] = 1;
        zeep3 [3] = 1;
        zeep3 [4] = 1;
        zeep3 [5] = 1;
        zeep3 [6] = 0;
        zeep3 [7] = 0;

        zeep4 [0] = 0;
        zeep4 [1] = 1;
        zeep4 [2] = 0;
        zeep4 [3] = 0;
        zeep4 [4] = 0;
        zeep4 [5] = 0;
        zeep4 [6] = 1;
        zeep4 [7] = 0;

        zeep5 [0] = 1;
        zeep5 [1] = 0;
        zeep5 [2] = 0;
        zeep5 [3] = 1;
        zeep5 [4] = 0;
        zeep5 [5] = 1;
        zeep5 [6] = 0;
        zeep5 [7] = 1;
           
        zeep6 [0] = 0;
        zeep6 [1] = 1;
        zeep6 [2] = 0;
        zeep6 [3] = 0;
        zeep6 [4] = 0;
        zeep6 [5] = 0;
        zeep6 [6] = 1;
        zeep6 [7] = 0;

        zeep7 [0] = 0;
        zeep7 [1] = 1;
        zeep7 [2] = 1;
        zeep7 [3] = 1;
        zeep7 [4] = 1;
        zeep7 [5] = 1;
        zeep7 [6] = 1;
        zeep7 [7] = 0;
    }

    public byte access2D(int i, int j){
        byte returnVal;
        returnVal = (byte) 0;
        if(i == 7)
            returnVal = (byte) zeep0[j];
        if(i == 6)
            returnVal = (byte) zeep1[j];
        if(i == 5)
            returnVal = (byte) zeep2[j];
        if(i == 4)
            returnVal = (byte) zeep3[j];
        if(i == 3)
            returnVal = (byte) zeep4[j];
        if(i == 2)
            returnVal = (byte) zeep5[j];
        if(i == 1)
            returnVal = (byte) zeep6[j];
        if(i == 0)
            returnVal = (byte) zeep7[j];
        return returnVal;
    }
}