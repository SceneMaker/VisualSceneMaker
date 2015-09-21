package de.dfki.vsm.players.util;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.font.TextAttribute;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.RoundRectangle2D;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JPanel;

public class Stickman extends JPanel {
     
        private boolean isScared;
        private boolean isShame;
        
	private final int mWidth;
	private final int mHeight;
	private final String mCharacterName;

	private int mPosX;
	private int mPosY;
	private float mSize;
	private float mHeadSize;
	private float mStickHeight;

	private boolean mSpeakTriggered;
	private String mSpeakText;
	private int mSpeakText_PosX;
	private int mSpeakText_PosY;
	// rendering issues
	private FontMetrics mFM = null;
	private int mFontWidthCorrection = 0;
	private int mFontSize = 14;

	private Ellipse2D mHead;

	private Ellipse2D mRightEye;
	private Ellipse2D mLeftEye;
	private Line2D mRightEyeBrow;
	private Line2D mLeftEyeBrow;

	private Line2D mStick;//body
	private Line2D mRightLeg;
	private Line2D mLeftLeg;
	private Line2D mRightUpperArm;
	private Ellipse2D mLeftElbow;
	private Ellipse2D mRightElbow;
	private Line2D mRightForeArm;
	private Line2D mLeftUpperArm;
	private Line2D mLeftForeArm;

	//private Arc2D mMouth;
        private CubicCurve2D mMouth;
        private Ellipse2D mScaredMouth;
        
        //shame face lines
        private Line2D.Double mLine1;
        private Line2D.Double mLine2;
        private Line2D.Double mLine3;
        private Line2D.Double mLine4;
        private Line2D.Double mLine5;
        private Line2D.Double mLine6;

	private int mHead_posX;
	private int mHead_posY;
	private int mStick_Y1;
	private int mStick_Y2;
	private int mLegs_Y1;
	private int mLegs_Y2;
	private int mLegs_X;
	private int mUpperArms_Y1;
	private int mForeArms_Y1;
	private int mForeArms_Y2;
	private int mUpperArms_Y2;
	private int mArms_X;
	private int mRightEye_posX;
	private int mLeftEye_posX;
	private int mLeftEyeBrow_X1;
	private int mLeftEyeBrow_Y1;
	private int mLeftEyeBrow_X2;
	private int mLeftEyeBrow_Y2;
	private int mRightEyeBrow_X1;
	private int mRightEyeBrow_Y1;
	private int mRightEyeBrow_X2;
	private int mRightEyeBrow_Y2;

	private int mEyes_posY;
	private int mEyesSize;

	private int mMouth_X;
	private int mMouth_Y;
        private int mContr1_X;
        private int mContr1_Y;
        private int mContr2_X;
        private int mContr2_Y;
        private int mMouth_X1;
        private int mMouth_Y1;
        
        private int mMouthSize;
	//private int mMouth_W;
	//private int mMouth_H;
	//private int mMouth_Start;
	//private int mMouth_Extend;
	//private int type;

	private final Color mForegroundColor = new Color(188, 188, 188);
	private final Color mBackgroundColor = new Color(49, 49, 49);

	public Stickman(String name, int width, int height) {

		mCharacterName = name;
		mWidth = width;
		mHeight = height;
		initValues();

		initCoordinates();
		drawBody();
		drawFace();

		setFocusable(true);
		requestFocusInWindow();
	}

	private void initValues() {
            
                isScared = false;
		mPosX = mWidth / 2;
		mPosY = (int) (mHeight * .30);
		mSize = (float) (0.8);
		mHeadSize = (90 * mSize);
		mStickHeight = (170 * mSize);
		mSpeakTriggered = false;
		mSpeakText = "";
		mSpeakText_PosX = 20;
		mSpeakText_PosY = 30;

		// PG added font-width calculatioin
		Map<TextAttribute, Object> map = new Hashtable<>();

		map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON);
		map.put(TextAttribute.FAMILY, Font.SANS_SERIF);

		map.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_DEMIBOLD);
		map.put(TextAttribute.SIZE, mFontSize);

		// Derive the font from the attribute map
		Font font = Font.getFont(map);
                // Derive the node's font metrics from the font
		mFM = getFontMetrics(font);

		// Set the output font to the updated font
		setFont(font);

	}

	@Override
	public void paintComponent(Graphics g) {

		setBackground(mBackgroundColor);
		setForeground(mForegroundColor);

		super.paintComponent(g);

		Graphics2D g2 = (Graphics2D) g;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		g2.fillOval(mHead_posX, mHead_posY, (int) mHeadSize, (int) mHeadSize);

		setForeground(mForegroundColor);
		g2.setStroke(new BasicStroke(7));

		// Draw Body
		g2.draw(mHead);
		g2.draw(mStick);
		g2.draw(mRightLeg);
		g2.draw(mLeftLeg);
		g2.draw(mRightUpperArm);
		g2.draw(mRightForeArm);
		g2.draw(mLeftUpperArm);
		g2.draw(mLeftForeArm);
		g2.draw(mLeftElbow);
		g2.fill(mLeftElbow);
		g2.draw(mRightElbow);
		g2.fill(mRightElbow);

		g2.setStroke(new BasicStroke(2));
		g2.setColor(mBackgroundColor);
		// Draw Face
		g2.draw(mLeftEyeBrow);
		g2.draw(mRightEyeBrow);
		g2.draw(mRightEye);
		g2.draw(mLeftEye);
               if(isScared){
                   g2.draw(mScaredMouth);
               }else{
		g2.draw(mMouth);}
               if(isShame){
                   g2.draw(mLine1);
                   g2.draw(mLine2);
                   g2.draw(mLine3);
                   g2.draw(mLine4);
                   g2.draw(mLine5);
                   g2.draw(mLine6);
               }

		// Character Name
		g2.setColor(Color.red);
		g2.drawString(mCharacterName, mHead_posX, mHeight - mSpeakText_PosY / 3);

		if (mSpeakTriggered) {
			g2.setColor(mForegroundColor);

			// PG added font-width calculation
			mFontWidthCorrection = mFM.stringWidth(mSpeakText);

			g2.fill((new RoundRectangle2D.Double(mSpeakText_PosX - 5, mSpeakText_PosY - 20, mFontWidthCorrection + mFontSize, 35, 15, 15)));
			g2.setColor(mBackgroundColor);
			g2.drawString(mSpeakText, mSpeakText_PosX, mSpeakText_PosY);
		}

		//g2.draw(mRightEyeBrow);
		//g2.draw(mLeftEyeBrow);
		//g2.draw(mMouth);
	}

	private void initCoordinates() {

		/* Set Neutral Position and Face */
		mHead_posX = (int) (mPosX - mHeadSize / 2);
		mHead_posY = (int) (mPosY - mHeadSize / 2);

		mStick_Y1 = (int) (mPosY + (mHeadSize / 2));
		mStick_Y2 = (int) (mPosY + mStickHeight);

		mLegs_Y1 = (int) (mPosY + mStickHeight);
		mLegs_Y2 = (int) (mPosY + (mStickHeight * 1.6));
		mLegs_X = (int) (mStickHeight / 5);

		mUpperArms_Y1 = (int) (mPosY + (mStickHeight / 2.3));
		mForeArms_Y1 = (int) (mPosY + mStickHeight / 0.9);
		mForeArms_Y2 = (int) (mPosY + mStickHeight / 2.5);
		mUpperArms_Y2 = (int) (mPosY + mStickHeight / 1.5);
		mArms_X = (int) (mStickHeight / 3.0);

		mLeftEyeBrow_X1 = (int) (mPosX - mHeadSize / 3.0);
		mLeftEyeBrow_Y1 = (int) (mPosY - mHeadSize / 3.0);
		mLeftEyeBrow_X2 = (int) (mPosX - mHeadSize / 10.0);
		mLeftEyeBrow_Y2 = (int) (mPosY - mHeadSize / 3.0);

		mRightEyeBrow_X1 = (int) (mPosX + mHeadSize / 15.0);
		mRightEyeBrow_Y1 = (int) (mPosY - mHeadSize / 3.0);
		mRightEyeBrow_X2 = (int) (mPosX + mHeadSize / 3.5);
		mRightEyeBrow_Y2 = (int) (mPosY - mHeadSize / 3.0);

		mRightEye_posX = (int) (mPosX + mHeadSize / 3.5 - (int) mHeadSize / 5);
		mLeftEye_posX = (int) (mPosX - mHeadSize / 3.5);
		mEyes_posY = (int) (mPosY - mHeadSize / 4);
		mEyesSize = (int) mHeadSize / 7;
                
                mMouthSize = (int) mHeadSize / 9;

		mMouth_X = (int) (mPosX - mHeadSize / 5.5);
		mMouth_Y = (int) (mPosY + mHeadSize / 6);
                mContr1_X = (int) (mPosX - mHeadSize / 5.5 + 8.3);
                mContr1_Y = (int) (mPosY + mHeadSize / 6 + 10);
                mContr2_X = (int) (mPosX - mHeadSize / 5.5 + 16.6);
                mContr2_Y = (int) (mPosY + mHeadSize / 6 + 10);
                mMouth_X1 = (int) (mPosX - mHeadSize / 5.5 + 25);
                mMouth_Y1 =(int) (mPosY + mHeadSize / 6);
                       
		//mMouth_W = 25;
		//mMouth_H = 15;
		//mMouth_Start = 0;
		//mMouth_Extend = -180;
		//type = Arc2D.OPEN;
	}

	private void drawBody() {

		mHead = new Ellipse2D.Double(mHead_posX, mHead_posY, (int) mHeadSize, (int) mHeadSize);

		mStick = new Line2D.Double(mPosX, mStick_Y1, mPosX, mStick_Y2);

		mRightLeg = new Line2D.Double(mPosX, mLegs_Y1, (mPosX + mLegs_X), mLegs_Y2);
		mLeftLeg = new Line2D.Double(mPosX, mLegs_Y1, (mPosX - mLegs_X), mLegs_Y2);

		mRightUpperArm = new Line2D.Double(mPosX, mUpperArms_Y1, (mPosX + mArms_X), mUpperArms_Y2);
		mRightElbow = new Ellipse2D.Double((mPosX + mArms_X * 0.95), mUpperArms_Y2 - 0.1, 6, 6);
		mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + mArms_X * 1.5), mForeArms_Y1);
		mLeftUpperArm = new Line2D.Double(mPosX, mUpperArms_Y1, (mPosX - mArms_X), mUpperArms_Y2);
		mLeftElbow = new Ellipse2D.Double((mPosX - mArms_X * 1.1), mUpperArms_Y2 - 0.1, 6, 6);
		mLeftForeArm = new Line2D.Double((mPosX - mArms_X), mUpperArms_Y2, (mPosX - mArms_X * 1.5), mForeArms_Y1);
                
                

	}

	private void drawFace() {

		// Face
		mRightEye = new Ellipse2D.Double(mRightEye_posX, mEyes_posY, mEyesSize, mEyesSize);
		mLeftEye = new Ellipse2D.Double(mLeftEye_posX, mEyes_posY, mEyesSize, mEyesSize);

		/*private int mLeftEyeBrow_X1;
		 private int mLeftEyeBrow_Y1;
		 private int mLeftEyeBrow_X2;
		 private int mLeftEyeBrow_Y2;
		 private int mRightEyeBrow_X1;
		 private int mRightEyeBrow_Y1;
		 private int mRightEyeBrow_X2;
		 private int mRightEyeBrow_Y2;*/
		// TODO
                mLine1 = new Line2D.Double(mPosX-mHeadSize/3,mPosY+mHeadSize / 6,mPosX-mHeadSize/3+3,mPosY+mHeadSize/12);
                mLine2 = new Line2D.Double(mPosX-mHeadSize/3+3,mPosY+mHeadSize / 6,mPosX-mHeadSize/3+3+3,mPosY+mHeadSize/12);
                mLine3 = new Line2D.Double(mPosX-mHeadSize/3+6,mPosY+mHeadSize / 6,mPosX-mHeadSize/3+3+3+3,mPosY+mHeadSize/12);
                mLine4 = new Line2D.Double(mPosX+mHeadSize/3-3,mPosY+mHeadSize / 6,mPosX+mHeadSize/3,mPosY+mHeadSize/12);
                mLine5 = new Line2D.Double(mPosX+mHeadSize/3-6,mPosY+mHeadSize / 6,mPosX+mHeadSize/3-3,mPosY+mHeadSize/12);
                mLine6 = new Line2D.Double(mPosX+mHeadSize/3-9,mPosY+mHeadSize / 6,mPosX+mHeadSize/3-6,mPosY+mHeadSize/12);
		mLeftEyeBrow = new Line2D.Double(mLeftEyeBrow_X1, mLeftEyeBrow_Y1, mLeftEyeBrow_X2, mLeftEyeBrow_Y2);
		mRightEyeBrow = new Line2D.Double(mRightEyeBrow_X1, mRightEyeBrow_Y1, mRightEyeBrow_X2, mRightEyeBrow_Y2);
		mMouth = new CubicCurve2D.Double(mMouth_X, mMouth_Y, mContr1_X, mContr1_Y, mContr2_X, mContr2_Y, mMouth_X1,mMouth_Y1);
                mScaredMouth = new Ellipse2D.Double(mPosX-mMouthSize,mPosY + mHeadSize / 6, 2*mMouthSize, 2*mMouthSize);
	}

	private void update() {
		this.repaint();
		try {
			Thread.sleep(10);
		} catch (InterruptedException ex) {
		}
	}

	/**
	 * ********************************************************************
	 * EMOTIONS
	 * *******************************************************************
	 */
	public void happy(double intensity) {
		// TODO: Modify face shapes based on the intensity value.
		// I say we modify the variables that are in the  
		// initCoordinates() method and then call drawFace()
                isScared = false;
                isShame = false;
               
                mEyesSize = (int) mHeadSize / 7;
                mLeftEyeBrow_Y2 = (int) (mPosY - mHeadSize / 3.0);
		mRightEyeBrow_Y1 = (int) (mPosY - mHeadSize / 3.0);
                mContr1_Y = (int) (mPosY + mHeadSize / 6 + 10 + intensity);
         
                mContr2_Y = (int) (mPosY + mHeadSize / 6 + 10 + intensity);
		drawFace();
		update();
	}

	public void sad(double intensity) {
		//mMouth_Extend = 180;
                isScared = false;
                isShame = false;
                
                mContr1_Y = (int) (mPosY + mHeadSize / 6 - 10 -intensity);
               
                mContr2_Y = (int) (mPosY + mHeadSize / 6 - 10 - intensity);
		mLeftEyeBrow_Y2 = (int) (mPosY - mHeadSize / 2.5 - intensity);
		mRightEyeBrow_Y1 = (int) (mPosY - mHeadSize / 2.5 -intensity);
		mEyesSize = (int) mHeadSize / 10;
		drawFace();
		update();
	}
        
        public void disgusted (double intensity){
                isScared = false;
                isShame = false;
                
                mContr1_Y = (int) (mPosY + mHeadSize / 6 + 15 +intensity);
                mContr2_Y = (int) (mPosY + mHeadSize / 6 - 15 +intensity);
                mLeftEyeBrow_Y2 = (int) (mPosY - mHeadSize / 2.5 -intensity);
		mRightEyeBrow_Y1 = (int) (mPosY - mHeadSize / 2.5 - intensity);
		mEyesSize = (int) mHeadSize / 10;
                drawFace();
		update();
        }

	public void scared(double intensity) {
		//mMouth_Start = 90;
		//mMouth_Extend = 360;
                isScared = true;
                isShame = false;
                
                mEyesSize = (int) (mHeadSize / 6.5+2*intensity);
                mMouthSize = (int) (mHeadSize / 9+ 2*intensity);
                mLeftEyeBrow_Y2 = (int) (mPosY - mHeadSize / 3.0);
		mRightEyeBrow_Y1 = (int) (mPosY - mHeadSize / 3.0);
                drawFace();
                update();
                
               
	}
        
        public void shame(double intensity) {
		
                isScared = true;
                isShame = true;
                
                mEyesSize = (int) (mHeadSize / 7);
                mMouthSize = (int) (mHeadSize / 20);
                mLeftEyeBrow_Y2 = (int) (mPosY - mHeadSize / 3.0);
		mRightEyeBrow_Y1 = (int) (mPosY - mHeadSize / 3.0);
                drawFace();
                update();
                
               
	}
        

	public void angry(double intensity) {
		//mMouth_Start = 0;
		//mMouth_Extend = 180;
		//mMouth_H = 1;
                isScared = false;
                isShame = false;
                mEyesSize = (int) mHeadSize / 7;
                mContr1_Y = (int) (mPosY + mHeadSize / 6 );
                mContr2_Y = (int) (mPosY + mHeadSize / 6 );
		mLeftEyeBrow_Y2 = (int) (mPosY - mHeadSize / 4.0 -2*intensity);
		mRightEyeBrow_Y1 = (int) (mPosY - mHeadSize / 4.0 -2*intensity);
		drawFace();
		update();
	}

	/**
	 * ********************************************************************
	 * ACTIONS
	 * *******************************************************************
	 */
	public void wave() {

		mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + mArms_X * 1.5), mForeArms_Y2);
		update();
		try {
			Thread.sleep(200);
		} catch (InterruptedException ex) {
			Logger.getLogger(Stickman.class.getName()).log(Level.SEVERE, null, ex);
		}
		mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + mArms_X * 1.5), mForeArms_Y1);
		update();

		/*int x = 100;
		 while (x>30) {
		 mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + x + mStickHeight/4.0), (mPosY + x));
		 x = x-2;
		 update();
		 }

		 while (x<100) {
		 mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + x + mStickHeight/4.0), (mPosY + x));
		 x =x+2;
		 update();
		 }*/
		mSpeakTriggered = false;
		update();
	}

	public void cup() {
		mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + mArms_X), (mPosY + mStickHeight*1.1));
		update();
                try {
			Thread.sleep(1000);
		} catch (InterruptedException ex) {
			Logger.getLogger(Stickman.class.getName()).log(Level.SEVERE, null, ex);
		}
                mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + mArms_X * 1.5), mForeArms_Y1);
                update();
                
	}

	public void scratch() {
		mRightUpperArm = new Line2D.Double(mPosX, mUpperArms_Y1, (mPosX + mArms_X * 1.5), (mPosY + mStickHeight / 3.5));
		mRightElbow = new Ellipse2D.Double((mPosX + mArms_X * 1.4), (mPosY + mStickHeight / 3.7), 6, 6);
		mRightForeArm = new Line2D.Double((mPosX + mArms_X * 1.5), (mPosY + mStickHeight / 3.5), (mPosX + mArms_X * 0.5), (mPosY - mStickHeight / 7));
		update();
                
                 try {
			Thread.sleep(1000);
		} catch (InterruptedException ex) {
			Logger.getLogger(Stickman.class.getName()).log(Level.SEVERE, null, ex);
		}
                 mRightUpperArm = new Line2D.Double(mPosX, mUpperArms_Y1, (mPosX + mArms_X), mUpperArms_Y2);
                 mRightElbow = new Ellipse2D.Double((mPosX + mArms_X * 0.95), mUpperArms_Y2 - 0.1, 6, 6);
                 mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + mArms_X * 1.5), mForeArms_Y1);
                 update();
                 
	}
        
        public void lookTo(int direction) {
            
           
            if(direction==1){
                System.out.println("Look to right ");
            }
            if(direction==-1){
                System.out.println("Look to left ");
            }
            // if direction = -1 look left
            // if direction = 1 look right
                 
	}

	public void box() {
		mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + mArms_X), (mPosY + mStickHeight));
		mLeftForeArm = new Line2D.Double((mPosX - mArms_X), mUpperArms_Y2, (mPosX - mArms_X), (mPosY + mStickHeight));
		update();
                
                  try {
			Thread.sleep(1000);
		} catch (InterruptedException ex) {
			Logger.getLogger(Stickman.class.getName()).log(Level.SEVERE, null, ex);
		}
                  
                mRightForeArm = new Line2D.Double((mPosX + mArms_X), mUpperArms_Y2, (mPosX + mArms_X * 1.5), mForeArms_Y1);
                mLeftForeArm = new Line2D.Double((mPosX - mArms_X), mUpperArms_Y2, (mPosX - mArms_X * 1.5), mForeArms_Y1);
                update();
	}

	/**
	 * ********************************************************************
	 * SPEAK 
         *******************************************************************
	 */
	public void speak(String msg) {

		mSpeakText = msg;
		mSpeakTriggered = true;
		update();

	}

}
