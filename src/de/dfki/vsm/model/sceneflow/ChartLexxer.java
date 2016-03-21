/* The following code was generated by JFlex 1.4.3 on 21.03.16 09:21 */

////////////////////////////////////////////////////////////////////////////////
// Start User Code /////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Local Package Definition
package de.dfki.vsm.model.sceneflow;
// Import Java Cup Runtime
import java_cup.runtime.Symbol;

////////////////////////////////////////////////////////////////////////////////
// End User Code ///////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.4.3
 * on 21.03.16 09:21 from the specification file
 * <tt>D:/GitHubOrg/VisualSceneMaker/Master/src/de/dfki/vsm/model/sceneflow/lexxer.jflex</tt>
 */
public final class ChartLexxer implements java_cup.runtime.Scanner {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0, 0
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\10\0\1\1\1\14\1\14\1\0\1\14\1\14\22\0\1\14\1\73"+
    "\1\15\3\1\1\71\1\1\1\12\1\13\1\5\1\4\1\100\1\6"+
    "\1\3\1\70\12\2\1\102\1\1\1\74\1\72\1\75\1\101\1\67"+
    "\1\36\1\66\1\52\1\33\1\57\1\50\1\26\1\61\1\55\2\66"+
    "\1\51\1\66\1\56\1\63\1\16\1\66\1\44\1\22\1\60\1\40"+
    "\1\62\4\66\1\10\1\1\1\11\1\0\1\66\1\0\1\20\1\41"+
    "\1\23\1\47\1\24\1\54\1\35\1\64\1\34\1\66\1\42\1\17"+
    "\1\45\1\25\1\30\1\32\1\66\1\27\1\43\1\37\1\31\1\46"+
    "\1\65\1\66\1\21\1\53\1\76\1\7\1\77\1\1\66\0\1\1"+
    "\12\0\3\1\1\0\1\1\1\0\6\1\2\0\2\1\4\0\1\1"+
    "\1\0\1\1\5\0\1\1\2\0\2\1\1\0\1\1\1\0\1\1"+
    "\1\0\6\1\2\0\2\1\4\0\1\1\1\0\1\1\4\0\2\1"+
    "\2\0\1\1\uff00\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\1\0\1\1\1\2\1\3\1\4\1\5\1\6\1\1"+
    "\1\7\1\10\1\11\1\12\1\13\1\1\23\14\1\15"+
    "\1\16\1\1\1\17\1\20\1\21\1\22\1\23\1\24"+
    "\1\25\1\26\1\27\1\0\1\30\1\0\1\31\6\14"+
    "\1\32\16\14\1\33\1\34\1\35\1\36\1\37\1\40"+
    "\2\14\1\41\1\14\1\42\20\14\1\43\1\44\3\14"+
    "\1\45\4\14\1\46\16\14\1\47\1\50\1\14\1\51"+
    "\11\14\1\52\1\53\6\14\1\54\1\14\1\55\4\14"+
    "\1\56\1\14\1\57\2\14\1\60\4\14\1\61\26\14"+
    "\1\62\12\14\1\63\15\14\1\64\13\14\1\65\7\14"+
    "\1\66\1\67\2\14\1\70\5\14\1\71\1\72\1\14"+
    "\1\73\7\14\1\74\1\75";

  private static int [] zzUnpackAction() {
    int [] result = new int[251];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\103\0\206\0\103\0\103\0\103\0\103\0\311"+
    "\0\103\0\103\0\103\0\103\0\103\0\u010c\0\u014f\0\u0192"+
    "\0\u01d5\0\u0218\0\u025b\0\u029e\0\u02e1\0\u0324\0\u0367\0\u03aa"+
    "\0\u03ed\0\u0430\0\u0473\0\u04b6\0\u04f9\0\u053c\0\u057f\0\u05c2"+
    "\0\u0605\0\103\0\103\0\u0648\0\u068b\0\u06ce\0\u0711\0\u0754"+
    "\0\103\0\103\0\103\0\103\0\103\0\u0797\0\103\0\u010c"+
    "\0\103\0\u07da\0\u081d\0\u0860\0\u08a3\0\u08e6\0\u0929\0\u0192"+
    "\0\u096c\0\u09af\0\u09f2\0\u0a35\0\u0a78\0\u0abb\0\u0afe\0\u0b41"+
    "\0\u0b84\0\u0bc7\0\u0c0a\0\u0c4d\0\u0c90\0\u0cd3\0\103\0\103"+
    "\0\103\0\103\0\103\0\u0797\0\u0d16\0\u0d59\0\u0192\0\u0d9c"+
    "\0\u0192\0\u0ddf\0\u0e22\0\u0e65\0\u0ea8\0\u0eeb\0\u0f2e\0\u0f71"+
    "\0\u0fb4\0\u0ff7\0\u103a\0\u107d\0\u10c0\0\u1103\0\u1146\0\u1189"+
    "\0\u11cc\0\u0192\0\u0192\0\u120f\0\u1252\0\u1295\0\u0192\0\u12d8"+
    "\0\u131b\0\u135e\0\u13a1\0\u0192\0\u13e4\0\u1427\0\u146a\0\u14ad"+
    "\0\u14f0\0\u1533\0\u1576\0\u15b9\0\u15fc\0\u163f\0\u1682\0\u16c5"+
    "\0\u1708\0\u174b\0\u0192\0\u0192\0\u178e\0\u0192\0\u17d1\0\u1814"+
    "\0\u1857\0\u189a\0\u18dd\0\u1920\0\u1963\0\u19a6\0\u19e9\0\u0192"+
    "\0\u1a2c\0\u1a6f\0\u1ab2\0\u1af5\0\u1b38\0\u1b7b\0\u1bbe\0\u0192"+
    "\0\u1c01\0\u0192\0\u1c44\0\u1c87\0\u1cca\0\u1d0d\0\u0192\0\u1d50"+
    "\0\u0192\0\u1d93\0\u1dd6\0\u0192\0\u1e19\0\u1e5c\0\u1e9f\0\u1ee2"+
    "\0\u0192\0\u1f25\0\u1f68\0\u1fab\0\u1fee\0\u2031\0\u2074\0\u20b7"+
    "\0\u20fa\0\u213d\0\u2180\0\u21c3\0\u2206\0\u2249\0\u228c\0\u22cf"+
    "\0\u2312\0\u2355\0\u2398\0\u23db\0\u241e\0\u2461\0\u24a4\0\u0192"+
    "\0\u24e7\0\u252a\0\u256d\0\u25b0\0\u25f3\0\u2636\0\u2679\0\u26bc"+
    "\0\u26ff\0\u2742\0\u0192\0\u2785\0\u27c8\0\u280b\0\u284e\0\u2891"+
    "\0\u28d4\0\u2917\0\u295a\0\u299d\0\u29e0\0\u2a23\0\u2a66\0\u2aa9"+
    "\0\u0192\0\u2aec\0\u2b2f\0\u2b72\0\u2bb5\0\u2bf8\0\u2c3b\0\u2c7e"+
    "\0\u2cc1\0\u2d04\0\u2d47\0\u2d8a\0\u0192\0\u2dcd\0\u2e10\0\u2e53"+
    "\0\u2e96\0\u2ed9\0\u2f1c\0\u2f5f\0\u0192\0\u0192\0\u2fa2\0\u2fe5"+
    "\0\u0192\0\u3028\0\u306b\0\u30ae\0\u30f1\0\u3134\0\u0192\0\u0192"+
    "\0\u3177\0\u0192\0\u31ba\0\u31fd\0\u3240\0\u3283\0\u32c6\0\u3309"+
    "\0\u334c\0\u0192\0\u0192";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[251];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\2\2\1\3\1\4\1\5\1\6\1\7\1\10\1\11"+
    "\1\12\1\13\1\14\1\15\1\16\1\17\3\20\1\21"+
    "\2\20\1\22\1\23\4\20\1\24\1\25\1\20\1\26"+
    "\1\27\1\30\3\20\1\31\3\20\1\32\1\33\1\34"+
    "\1\20\1\35\1\25\1\20\1\36\1\37\1\40\1\41"+
    "\4\20\1\42\1\43\1\44\1\45\1\46\1\47\1\50"+
    "\1\51\1\52\1\53\1\54\1\55\105\0\1\3\1\56"+
    "\106\0\1\57\74\0\14\60\1\61\60\60\2\0\3\60"+
    "\2\0\1\20\13\0\1\20\1\62\47\20\16\0\1\20"+
    "\13\0\51\20\16\0\1\20\13\0\16\20\1\63\32\20"+
    "\16\0\1\20\13\0\6\20\1\64\4\20\1\65\35\20"+
    "\16\0\1\20\13\0\6\20\1\66\42\20\16\0\1\20"+
    "\13\0\6\20\1\67\42\20\16\0\1\20\13\0\7\20"+
    "\1\70\30\20\1\70\10\20\16\0\1\20\13\0\31\20"+
    "\1\71\17\20\16\0\1\20\13\0\11\20\1\72\37\20"+
    "\16\0\1\20\13\0\7\20\1\73\41\20\16\0\1\20"+
    "\13\0\2\20\1\74\3\20\1\75\42\20\16\0\1\20"+
    "\13\0\16\20\1\76\32\20\16\0\1\20\13\0\2\20"+
    "\1\77\46\20\16\0\1\20\13\0\1\20\1\100\10\20"+
    "\1\101\36\20\16\0\1\20\13\0\2\20\1\102\46\20"+
    "\16\0\1\20\13\0\27\20\1\103\21\20\16\0\1\20"+
    "\13\0\16\20\1\104\32\20\16\0\1\20\13\0\16\20"+
    "\1\105\32\20\16\0\1\20\13\0\2\20\1\106\46\20"+
    "\105\0\1\107\103\0\1\110\102\0\1\111\102\0\1\112"+
    "\102\0\1\113\12\0\1\114\102\0\1\20\13\0\2\20"+
    "\1\115\46\20\16\0\1\20\13\0\35\20\1\116\13\20"+
    "\16\0\1\20\13\0\47\20\1\117\1\20\16\0\1\20"+
    "\13\0\1\20\1\120\47\20\16\0\1\20\13\0\21\20"+
    "\1\121\27\20\16\0\1\20\13\0\36\20\1\122\12\20"+
    "\16\0\1\20\13\0\31\20\1\123\17\20\16\0\1\20"+
    "\13\0\13\20\1\124\35\20\16\0\1\20\13\0\23\20"+
    "\1\125\25\20\16\0\1\20\13\0\7\20\1\126\41\20"+
    "\16\0\1\20\13\0\27\20\1\127\21\20\16\0\1\20"+
    "\13\0\11\20\1\130\37\20\16\0\1\20\13\0\25\20"+
    "\1\131\23\20\16\0\1\20\13\0\6\20\1\132\42\20"+
    "\16\0\1\20\13\0\7\20\1\133\41\20\16\0\1\20"+
    "\13\0\1\20\1\134\47\20\16\0\1\20\13\0\14\20"+
    "\1\135\34\20\16\0\1\20\13\0\27\20\1\136\21\20"+
    "\16\0\1\20\13\0\25\20\1\137\23\20\16\0\1\20"+
    "\13\0\1\20\1\140\47\20\16\0\1\20\13\0\3\20"+
    "\1\141\45\20\16\0\1\20\13\0\6\20\1\142\42\20"+
    "\16\0\1\20\13\0\1\20\1\143\47\20\16\0\1\20"+
    "\13\0\2\20\1\144\46\20\16\0\1\20\13\0\32\20"+
    "\1\145\1\146\15\20\16\0\1\20\13\0\6\20\1\147"+
    "\42\20\16\0\1\20\13\0\1\20\1\150\47\20\16\0"+
    "\1\20\13\0\31\20\1\151\17\20\16\0\1\20\13\0"+
    "\12\20\1\152\36\20\16\0\1\20\13\0\25\20\1\153"+
    "\23\20\16\0\1\20\13\0\21\20\1\154\27\20\16\0"+
    "\1\20\13\0\2\20\1\155\46\20\16\0\1\20\13\0"+
    "\21\20\1\156\27\20\16\0\1\20\13\0\25\20\1\124"+
    "\23\20\16\0\1\20\13\0\21\20\1\157\27\20\16\0"+
    "\1\20\13\0\6\20\1\160\42\20\16\0\1\20\13\0"+
    "\21\20\1\161\27\20\16\0\1\20\13\0\13\20\1\162"+
    "\35\20\16\0\1\20\13\0\4\20\1\163\10\20\1\164"+
    "\33\20\16\0\1\20\13\0\13\20\1\165\35\20\16\0"+
    "\1\20\13\0\16\20\1\166\32\20\16\0\1\20\13\0"+
    "\2\20\1\167\46\20\16\0\1\20\13\0\12\20\1\170"+
    "\36\20\16\0\1\20\13\0\12\20\1\171\36\20\16\0"+
    "\1\20\13\0\30\20\1\172\20\20\16\0\1\20\13\0"+
    "\21\20\1\173\27\20\16\0\1\20\13\0\11\20\1\174"+
    "\37\20\16\0\1\20\13\0\2\20\1\175\46\20\16\0"+
    "\1\20\13\0\3\20\1\176\45\20\16\0\1\20\13\0"+
    "\12\20\1\177\36\20\16\0\1\20\13\0\12\20\1\200"+
    "\36\20\16\0\1\20\13\0\6\20\1\201\42\20\16\0"+
    "\1\20\13\0\5\20\1\202\43\20\16\0\1\20\13\0"+
    "\16\20\1\203\32\20\16\0\1\20\13\0\1\20\1\204"+
    "\47\20\16\0\1\20\13\0\11\20\1\205\37\20\16\0"+
    "\1\20\13\0\25\20\1\206\23\20\16\0\1\20\13\0"+
    "\5\20\1\207\43\20\16\0\1\20\13\0\27\20\1\210"+
    "\21\20\16\0\1\20\13\0\6\20\1\211\42\20\16\0"+
    "\1\20\13\0\16\20\1\212\32\20\16\0\1\20\13\0"+
    "\13\20\1\213\35\20\16\0\1\20\13\0\11\20\1\214"+
    "\37\20\16\0\1\20\13\0\45\20\1\215\3\20\16\0"+
    "\1\20\13\0\6\20\1\216\42\20\16\0\1\20\13\0"+
    "\2\20\1\217\46\20\16\0\1\20\13\0\21\20\1\220"+
    "\27\20\16\0\1\20\13\0\25\20\1\221\23\20\16\0"+
    "\1\20\13\0\21\20\1\222\27\20\16\0\1\20\13\0"+
    "\24\20\1\223\24\20\16\0\1\20\13\0\32\20\1\224"+
    "\1\225\15\20\16\0\1\20\13\0\7\20\1\226\41\20"+
    "\16\0\1\20\13\0\21\20\1\227\27\20\16\0\1\20"+
    "\13\0\3\20\1\230\45\20\16\0\1\20\13\0\36\20"+
    "\1\231\12\20\16\0\1\20\13\0\7\20\1\232\41\20"+
    "\16\0\1\20\13\0\1\20\1\233\47\20\16\0\1\20"+
    "\13\0\21\20\1\234\27\20\16\0\1\20\13\0\4\20"+
    "\1\235\13\20\1\236\30\20\16\0\1\20\13\0\16\20"+
    "\1\237\32\20\16\0\1\20\13\0\2\20\1\240\46\20"+
    "\16\0\1\20\13\0\25\20\1\241\23\20\16\0\1\20"+
    "\13\0\4\20\1\242\10\20\1\243\10\20\1\244\5\20"+
    "\1\245\7\20\1\246\4\20\16\0\1\20\13\0\6\20"+
    "\1\247\42\20\16\0\1\20\13\0\12\20\1\250\36\20"+
    "\16\0\1\20\13\0\5\20\1\251\43\20\16\0\1\20"+
    "\13\0\1\20\1\252\47\20\16\0\1\20\13\0\11\20"+
    "\1\253\37\20\16\0\1\20\13\0\25\20\1\254\23\20"+
    "\16\0\1\20\13\0\6\20\1\255\42\20\16\0\1\20"+
    "\13\0\6\20\1\256\42\20\16\0\1\20\13\0\13\20"+
    "\1\257\35\20\16\0\1\20\13\0\1\20\1\260\10\20"+
    "\1\261\36\20\16\0\1\20\13\0\2\20\1\262\46\20"+
    "\16\0\1\20\13\0\10\20\1\263\40\20\16\0\1\20"+
    "\13\0\17\20\1\264\31\20\16\0\1\20\13\0\6\20"+
    "\1\265\42\20\16\0\1\20\13\0\1\20\1\266\47\20"+
    "\16\0\1\20\13\0\25\20\1\267\23\20\16\0\1\20"+
    "\13\0\21\20\1\270\27\20\16\0\1\20\13\0\21\20"+
    "\1\271\27\20\16\0\1\20\13\0\6\20\1\272\42\20"+
    "\16\0\1\20\13\0\7\20\1\273\41\20\16\0\1\20"+
    "\13\0\6\20\1\274\42\20\16\0\1\20\13\0\7\20"+
    "\1\275\41\20\16\0\1\20\13\0\1\20\1\276\47\20"+
    "\16\0\1\20\13\0\11\20\1\277\37\20\16\0\1\20"+
    "\13\0\13\20\1\300\35\20\16\0\1\20\13\0\7\20"+
    "\1\301\41\20\16\0\1\20\13\0\4\20\1\302\44\20"+
    "\16\0\1\20\13\0\21\20\1\303\27\20\16\0\1\20"+
    "\13\0\15\20\1\304\33\20\16\0\1\20\13\0\14\20"+
    "\1\305\34\20\16\0\1\20\13\0\42\20\1\306\6\20"+
    "\16\0\1\20\13\0\2\20\1\307\46\20\16\0\1\20"+
    "\13\0\21\20\1\310\27\20\16\0\1\20\13\0\13\20"+
    "\1\311\35\20\16\0\1\20\13\0\12\20\1\312\36\20"+
    "\16\0\1\20\13\0\6\20\1\313\42\20\16\0\1\20"+
    "\13\0\6\20\1\314\42\20\16\0\1\20\13\0\5\20"+
    "\1\315\43\20\16\0\1\20\13\0\6\20\1\316\42\20"+
    "\16\0\1\20\13\0\34\20\1\317\14\20\16\0\1\20"+
    "\13\0\16\20\1\320\32\20\16\0\1\20\13\0\11\20"+
    "\1\321\37\20\16\0\1\20\13\0\2\20\1\322\46\20"+
    "\16\0\1\20\13\0\6\20\1\323\42\20\16\0\1\20"+
    "\13\0\13\20\1\324\35\20\16\0\1\20\13\0\20\20"+
    "\1\325\30\20\16\0\1\20\13\0\10\20\1\326\40\20"+
    "\16\0\1\20\13\0\6\20\1\327\42\20\16\0\1\20"+
    "\13\0\14\20\1\330\34\20\16\0\1\20\13\0\1\20"+
    "\1\331\47\20\16\0\1\20\13\0\27\20\1\332\21\20"+
    "\16\0\1\20\13\0\16\20\1\333\32\20\16\0\1\20"+
    "\13\0\45\20\1\334\3\20\16\0\1\20\13\0\14\20"+
    "\1\335\34\20\16\0\1\20\13\0\5\20\1\336\43\20"+
    "\16\0\1\20\13\0\11\20\1\337\37\20\16\0\1\20"+
    "\13\0\7\20\1\340\41\20\16\0\1\20\13\0\21\20"+
    "\1\341\27\20\16\0\1\20\13\0\6\20\1\342\42\20"+
    "\16\0\1\20\13\0\6\20\1\343\42\20\16\0\1\20"+
    "\13\0\7\20\1\344\41\20\16\0\1\20\13\0\36\20"+
    "\1\345\12\20\16\0\1\20\13\0\21\20\1\346\27\20"+
    "\16\0\1\20\13\0\12\20\1\347\36\20\16\0\1\20"+
    "\13\0\6\20\1\350\42\20\16\0\1\20\13\0\46\20"+
    "\1\351\2\20\16\0\1\20\13\0\2\20\1\352\46\20"+
    "\16\0\1\20\13\0\45\20\1\353\3\20\16\0\1\20"+
    "\13\0\25\20\1\354\23\20\16\0\1\20\13\0\13\20"+
    "\1\355\35\20\16\0\1\20\13\0\10\20\1\356\40\20"+
    "\16\0\1\20\13\0\11\20\1\357\37\20\16\0\1\20"+
    "\13\0\36\20\1\360\12\20\16\0\1\20\13\0\4\20"+
    "\1\361\44\20\16\0\1\20\13\0\14\20\1\362\34\20"+
    "\16\0\1\20\13\0\11\20\1\363\37\20\16\0\1\20"+
    "\13\0\21\20\1\364\27\20\16\0\1\20\13\0\12\20"+
    "\1\365\36\20\16\0\1\20\13\0\2\20\1\366\46\20"+
    "\16\0\1\20\13\0\13\20\1\367\35\20\16\0\1\20"+
    "\13\0\21\20\1\370\27\20\16\0\1\20\13\0\14\20"+
    "\1\371\34\20\16\0\1\20\13\0\6\20\1\372\42\20"+
    "\16\0\1\20\13\0\25\20\1\373\23\20\14\0";

  private static int [] zzUnpackTrans() {
    int [] result = new int[13199];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unkown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\1\0\1\11\1\1\4\11\1\1\5\11\24\1\2\11"+
    "\5\1\5\11\1\0\1\11\1\0\1\11\25\1\5\11"+
    "\260\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[251];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the input device */
  private java.io.Reader zzReader;

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /** number of newlines encountered up to the start of the matched text */
  private int yyline;

  /** the number of characters up to the start of the matched text */
  private int yychar;

  /**
   * the number of characters from the last newline up to the start of the 
   * matched text
   */
  private int yycolumn;

  /** 
   * zzAtBOL == true <=> the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true <=> the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;


  /**
   * Creates a new scanner
   * There is also a java.io.InputStream version of this constructor.
   *
   * @param   in  the java.io.Reader to read input from.
   */
  public ChartLexxer(java.io.Reader in) {
    this.zzReader = in;
  }

  /**
   * Creates a new scanner.
   * There is also java.io.Reader version of this constructor.
   *
   * @param   in  the java.io.Inputstream to read input from.
   */
  public ChartLexxer(java.io.InputStream in) {
    this(new java.io.InputStreamReader(in));
  }

  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x10000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 246) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }


  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   * 
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {

    /* first: make room (if you can) */
    if (zzStartRead > 0) {
      System.arraycopy(zzBuffer, zzStartRead,
                       zzBuffer, 0,
                       zzEndRead-zzStartRead);

      /* translate stored positions */
      zzEndRead-= zzStartRead;
      zzCurrentPos-= zzStartRead;
      zzMarkedPos-= zzStartRead;
      zzStartRead = 0;
    }

    /* is the buffer big enough? */
    if (zzCurrentPos >= zzBuffer.length) {
      /* if not: blow it up */
      char newBuffer[] = new char[zzCurrentPos*2];
      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
      zzBuffer = newBuffer;
    }

    /* finally: fill the buffer with new input */
    int numRead = zzReader.read(zzBuffer, zzEndRead,
                                            zzBuffer.length-zzEndRead);

    if (numRead > 0) {
      zzEndRead+= numRead;
      return false;
    }
    // unlikely but not impossible: read 0 characters, but not at end of stream    
    if (numRead == 0) {
      int c = zzReader.read();
      if (c == -1) {
        return true;
      } else {
        zzBuffer[zzEndRead++] = (char) c;
        return false;
      }     
    }

	// numRead < 0
    return true;
  }

    
  /**
   * Closes the input stream.
   */
  public final void yyclose() throws java.io.IOException {
    zzAtEOF = true;            /* indicate end of file */
    zzEndRead = zzStartRead;  /* invalidate buffer    */

    if (zzReader != null)
      zzReader.close();
  }


  /**
   * Resets the scanner to read from a new input stream.
   * Does not close the old reader.
   *
   * All internal variables are reset, the old input stream 
   * <b>cannot</b> be reused (internal buffer is discarded and lost).
   * Lexical state is set to <tt>ZZ_INITIAL</tt>.
   *
   * @param reader   the new input stream 
   */
  public final void yyreset(java.io.Reader reader) {
    zzReader = reader;
    zzAtBOL  = true;
    zzAtEOF  = false;
    zzEOFDone = false;
    zzEndRead = zzStartRead = 0;
    zzCurrentPos = zzMarkedPos = 0;
    yyline = yychar = yycolumn = 0;
    zzLexicalState = YYINITIAL;
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final String yytext() {
    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
  }


  /**
   * Returns the character at position <tt>pos</tt> from the 
   * matched text. 
   * 
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch. 
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer[zzStartRead+pos];
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of 
   * yypushback(int) and a match-all fallback rule) this method 
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  } 


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public java_cup.runtime.Symbol next_token() throws java.io.IOException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    char [] zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      yychar+= zzMarkedPosL-zzStartRead;

      boolean zzR = false;
      for (zzCurrentPosL = zzStartRead; zzCurrentPosL < zzMarkedPosL;
                                                             zzCurrentPosL++) {
        switch (zzBufferL[zzCurrentPosL]) {
        case '\u000B':
        case '\u000C':
        case '\u0085':
        case '\u2028':
        case '\u2029':
          yyline++;
          yycolumn = 0;
          zzR = false;
          break;
        case '\r':
          yyline++;
          yycolumn = 0;
          zzR = true;
          break;
        case '\n':
          if (zzR)
            zzR = false;
          else {
            yyline++;
            yycolumn = 0;
          }
          break;
        default:
          zzR = false;
          yycolumn++;
        }
      }

      if (zzR) {
        // peek one character ahead if it is \n (if we have counted one line too much)
        boolean zzPeek;
        if (zzMarkedPosL < zzEndReadL)
          zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        else if (zzAtEOF)
          zzPeek = false;
        else {
          boolean eof = zzRefill();
          zzEndReadL = zzEndRead;
          zzMarkedPosL = zzMarkedPos;
          zzBufferL = zzBuffer;
          if (eof) 
            zzPeek = false;
          else 
            zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        }
        if (zzPeek) yyline--;
      }
      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      zzState = ZZ_LEXSTATE[zzLexicalState];


      zzForAction: {
        while (true) {
    
          if (zzCurrentPosL < zzEndReadL)
            zzInput = zzBufferL[zzCurrentPosL++];
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = zzBufferL[zzCurrentPosL++];
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          int zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
        case 50: 
          { return new Symbol(ChartFields.REMOVELAST);
          }
        case 62: break;
        case 24: 
          { return new Symbol(ChartFields.OR);
          }
        case 63: break;
        case 26: 
          { return new Symbol(ChartFields.IN);
          }
        case 64: break;
        case 60: 
          { return new Symbol(ChartFields.HISTORYCONTAINSSTATE);
          }
        case 65: break;
        case 13: 
          { return new Symbol(ChartFields.AT);
          }
        case 66: break;
        case 21: 
          { return new Symbol(ChartFields.COMMA);
          }
        case 67: break;
        case 61: 
          { return new Symbol(ChartFields.UASG);
          }
        case 68: break;
        case 18: 
          { return new Symbol(ChartFields.GREATER);
          }
        case 69: break;
        case 47: 
          { return new Symbol(ChartFields.VALUEOF);
          }
        case 70: break;
        case 59: 
          { return new Symbol(ChartFields.USG);
          }
        case 71: break;
        case 7: 
          { return new Symbol(ChartFields.LBRACK);
          }
        case 72: break;
        case 44: 
          { return new Symbol(ChartFields.DEFAULT);
          }
        case 73: break;
        case 9: 
          { return new Symbol(ChartFields.LPAREN);
          }
        case 74: break;
        case 51: 
          { return new Symbol(ChartFields.REMOVEFIRST);
          }
        case 75: break;
        case 11: 
          { /* ignore white space. */
          }
        case 76: break;
        case 41: 
          { return new Symbol(ChartFields.EMPTY);
          }
        case 77: break;
        case 20: 
          { return new Symbol(ChartFields.RBRACE);
          }
        case 78: break;
        case 31: 
          { return new Symbol(ChartFields.GREATEREQUAL);
          }
        case 79: break;
        case 56: 
          { return new Symbol(ChartFields.HISTORYSETDEPTH);
          }
        case 80: break;
        case 42: 
          { return new Symbol(ChartFields.RANDOM);
          }
        case 81: break;
        case 32: 
          { return new Symbol(ChartFields.FLOAT, new java.lang.Float(yytext()));
          }
        case 82: break;
        case 22: 
          { return new Symbol(ChartFields.QUESTION);
          }
        case 83: break;
        case 33: 
          { return new Symbol(ChartFields.NEW);
          }
        case 84: break;
        case 53: 
          { return new Symbol(ChartFields.PSG);
          }
        case 85: break;
        case 48: 
          { return new Symbol(ChartFields.ADDFIRST);
          }
        case 86: break;
        case 12: 
          { return new Symbol(ChartFields.VARIABLE, new java.lang.String(yytext()));
          }
        case 87: break;
        case 36: 
          { return new Symbol(ChartFields.NULL);
          }
        case 88: break;
        case 8: 
          { return new Symbol(ChartFields.RBRACK);
          }
        case 89: break;
        case 37: 
          { return new Symbol(ChartFields.BOOLEAN, new java.lang.Boolean(yytext()));
          }
        case 90: break;
        case 3: 
          { return new Symbol(ChartFields.DOT);
          }
        case 91: break;
        case 57: 
          { return new Symbol(ChartFields.HISTORYDEEPCLEAR);
          }
        case 92: break;
        case 5: 
          { return new Symbol(ChartFields.TIMES);
          }
        case 93: break;
        case 23: 
          { return new Symbol(ChartFields.COLON);
          }
        case 94: break;
        case 10: 
          { return new Symbol(ChartFields.RPAREN);
          }
        case 95: break;
        case 43: 
          { return new Symbol(ChartFields.REMOVE);
          }
        case 96: break;
        case 15: 
          { return new Symbol(ChartFields.EQUAL);
          }
        case 97: break;
        case 34: 
          { return new Symbol(ChartFields.GET);
          }
        case 98: break;
        case 46: 
          { return new Symbol(ChartFields.TIMEOUT);
          }
        case 99: break;
        case 52: 
          { return new Symbol(ChartFields.HISTORYCLEAR);
          }
        case 100: break;
        case 30: 
          { return new Symbol(ChartFields.LESSEQUAL);
          }
        case 101: break;
        case 54: 
          { return new Symbol(ChartFields.HISTORYVALUEOF);
          }
        case 102: break;
        case 45: 
          { return new Symbol(ChartFields.ADDLAST);
          }
        case 103: break;
        case 40: 
          { return new Symbol(ChartFields.CLEAR);
          }
        case 104: break;
        case 19: 
          { return new Symbol(ChartFields.LBRACE);
          }
        case 105: break;
        case 28: 
          { return new Symbol(ChartFields.EQUALEQUAL);
          }
        case 106: break;
        case 1: 
          { System.err.println("Illegal character: "+yytext());
          }
        case 107: break;
        case 49: 
          { return new Symbol(ChartFields.CONTAINS);
          }
        case 108: break;
        case 39: 
          { return new Symbol(ChartFields.FIRST);
          }
        case 109: break;
        case 16: 
          { return new Symbol(ChartFields.NOT);
          }
        case 110: break;
        case 27: 
          { return new Symbol(ChartFields.AND);
          }
        case 111: break;
        case 58: 
          { return new Symbol(ChartFields.HISTORYRUNTIMEOF);
          }
        case 112: break;
        case 25: 
          { return new Symbol(ChartFields.STRING, new java.lang.String(yytext()));
          }
        case 113: break;
        case 6: 
          { return new Symbol(ChartFields.MINUS);
          }
        case 114: break;
        case 14: 
          { return new Symbol(ChartFields.DIV);
          }
        case 115: break;
        case 4: 
          { return new Symbol(ChartFields.PLUS);
          }
        case 116: break;
        case 38: 
          { return new Symbol(ChartFields.LAST);
          }
        case 117: break;
        case 17: 
          { return new Symbol(ChartFields.LESS);
          }
        case 118: break;
        case 2: 
          { return new Symbol(ChartFields.INTEGER, new java.lang.Integer(yytext()));
          }
        case 119: break;
        case 35: 
          { return new Symbol(ChartFields.SIZE);
          }
        case 120: break;
        case 29: 
          { return new Symbol(ChartFields.NOTEQUAL);
          }
        case 121: break;
        case 55: 
          { return new Symbol(ChartFields.PDA);
          }
        case 122: break;
        default: 
          if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
            zzAtEOF = true;
            return null;
          } 
          else {
            zzScanError(ZZ_NO_MATCH);
          }
      }
    }
  }


}
