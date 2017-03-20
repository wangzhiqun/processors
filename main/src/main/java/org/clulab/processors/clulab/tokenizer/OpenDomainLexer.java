// Generated from main/src/main/java/org/clulab/processors/clulab/tokenizer/OpenDomainLexer.g by ANTLR 4.6

  package org.clulab.processors.clulab.tokenizer;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class OpenDomainLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.6", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		PARENS=1, QUOTES=2, DATE=3, NUMBER=4, FRACTION=5, WORD=6, TWITTER_NAME=7, 
		TWITTER_HASHTAG=8, FILENAME=9, PROGRAMMING_LANGUAGES=10, FULLURL=11, LIKELYURL_WWW=12, 
		LIKELYURL_COM=13, EMAIL=14, EOS=15, WHITESPACE=16, ErrorCharacter=17;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"PARENS", "QUOTES", "DATE", "NUMBER", "FRACTION", "WORD", "TWITTER_NAME", 
		"TWITTER_HASHTAG", "FILENAME", "PROGRAMMING_LANGUAGES", "FULLURL", "LIKELYURL_WWW", 
		"LIKELYURL_COM", "EMAIL", "EOS", "WHITESPACE", "ErrorCharacter", "LOWER_CASE_LETTER", 
		"UPPER_CASE_LETTER", "SPLET", "LETTER", "DIGIT", "ALPHANUM", "NUM", "ONE_TO_TWO_DIGITS", 
		"TWO_TO_FOUR_DIGITS", "ONE_TO_FOUR_DIGITS", "PUNCTUATION", "FILENAME_EXT", 
		"URL_BLOCK1", "URL_BLOCK2", "URL_BLOCK3", "URL_END1", "URL_END2", "URL_END3", 
		"EMAIL_USER", "EMAIL_DOMAIN"
	};

	private static final String[] _LITERAL_NAMES = {
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "PARENS", "QUOTES", "DATE", "NUMBER", "FRACTION", "WORD", "TWITTER_NAME", 
		"TWITTER_HASHTAG", "FILENAME", "PROGRAMMING_LANGUAGES", "FULLURL", "LIKELYURL_WWW", 
		"LIKELYURL_COM", "EMAIL", "EOS", "WHITESPACE", "ErrorCharacter"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public OpenDomainLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "OpenDomainLexer.g"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\23\u022a\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t"+
		" \4!\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3"+
		"\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2"+
		"\3\2\3\2\3\2\3\2\3\2\5\2l\n\2\3\3\3\3\3\3\3\3\3\3\3\3\5\3t\n\3\3\4\3\4"+
		"\3\4\3\4\3\4\3\4\3\5\5\5}\n\5\3\5\3\5\3\6\3\6\3\6\3\6\3\7\6\7\u0086\n"+
		"\7\r\7\16\7\u0087\3\7\3\7\6\7\u008c\n\7\r\7\16\7\u008d\7\7\u0090\n\7\f"+
		"\7\16\7\u0093\13\7\3\b\3\b\3\b\3\b\5\b\u0099\n\b\3\b\3\b\3\b\3\b\7\b\u009f"+
		"\n\b\f\b\16\b\u00a2\13\b\3\t\3\t\6\t\u00a6\n\t\r\t\16\t\u00a7\3\n\6\n"+
		"\u00ab\n\n\r\n\16\n\u00ac\3\n\3\n\6\n\u00b1\n\n\r\n\16\n\u00b2\7\n\u00b5"+
		"\n\n\f\n\16\n\u00b8\13\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13"+
		"\5\13\u00c4\n\13\3\f\3\f\3\f\3\f\3\f\3\f\5\f\u00cc\n\f\3\f\3\f\3\f\3\f"+
		"\3\f\6\f\u00d3\n\f\r\f\16\f\u00d4\3\f\3\f\3\r\3\r\3\r\3\r\3\r\3\r\6\r"+
		"\u00df\n\r\r\r\16\r\u00e0\3\r\3\r\6\r\u00e5\n\r\r\r\16\r\u00e6\3\r\3\r"+
		"\5\r\u00eb\n\r\3\r\3\r\5\r\u00ef\n\r\3\r\3\r\5\r\u00f3\n\r\3\r\3\r\5\r"+
		"\u00f7\n\r\3\16\6\16\u00fa\n\16\r\16\16\16\u00fb\3\16\3\16\6\16\u0100"+
		"\n\16\r\16\16\16\u0101\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3"+
		"\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\5\16\u0118\n\16"+
		"\3\16\3\16\6\16\u011c\n\16\r\16\16\16\u011d\3\16\3\16\5\16\u0122\n\16"+
		"\3\17\3\17\3\17\3\17\3\17\5\17\u0129\n\17\3\17\3\17\3\17\3\17\3\17\7\17"+
		"\u0130\n\17\f\17\16\17\u0133\13\17\3\17\3\17\3\17\3\17\3\17\3\17\5\17"+
		"\u013b\n\17\3\20\6\20\u013e\n\20\r\20\16\20\u013f\3\21\6\21\u0143\n\21"+
		"\r\21\16\21\u0144\3\21\3\21\3\22\3\22\3\23\3\23\3\24\3\24\3\25\3\25\3"+
		"\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\5\25\u015e"+
		"\n\25\3\26\3\26\3\26\3\26\3\26\3\26\3\26\5\26\u0167\n\26\3\27\5\27\u016a"+
		"\n\27\3\30\3\30\5\30\u016e\n\30\3\31\6\31\u0171\n\31\r\31\16\31\u0172"+
		"\3\31\7\31\u0176\n\31\f\31\16\31\u0179\13\31\3\31\3\31\6\31\u017d\n\31"+
		"\r\31\16\31\u017e\6\31\u0181\n\31\r\31\16\31\u0182\5\31\u0185\n\31\3\32"+
		"\3\32\5\32\u0189\n\32\3\33\3\33\3\33\5\33\u018e\n\33\3\33\5\33\u0191\n"+
		"\33\3\34\3\34\5\34\u0195\n\34\3\34\5\34\u0198\n\34\3\34\5\34\u019b\n\34"+
		"\3\35\3\35\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\5\36\u020f\n\36\3\37\3\37\3 \3 \3!\3!\3\"\3\"\3#\3#\3$\3$\3"+
		"%\5%\u021e\n%\3%\7%\u0221\n%\f%\16%\u0224\13%\3&\6&\u0227\n&\r&\16&\u0228"+
		"\2\2\'\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17"+
		"\35\20\37\21!\22#\23%\2\'\2)\2+\2-\2/\2\61\2\63\2\65\2\67\29\2;\2=\2?"+
		"\2A\2C\2E\2G\2I\2K\2\3\2\30\4\2\u201a\u201b\u201e\u201f\4\2//\61\61\4"+
		"\2--//\4\2\61\61\u2046\u2046\7\2##))/\60AAaa\4\2EEee\4\2HHhh\t\2\13\17"+
		"\"\"\u0087\u0087\u00a2\u00a2\u2002\u200c\u202a\u202b\u3002\u3002\f\2C"+
		"CGGKKQQWWccggkkqqww.\2\u00af\u00af\u0239\u0251\u02c4\u02c7\u02d4\u02e1"+
		"\u02e7\u037f\u0386\u0387\u03d1\u03d1\u03f8\u03f8\u03fe\u0401\u0485\u0489"+
		"\u04d1\u04d1\u04f8\u0501\u0512\u0527\u055c\u0561\u0593\u05bf\u05c1\u05c1"+
		"\u05c3\u05c4\u05c6\u05c7\u05c9\u05c9\u0617\u061c\u063d\u0641\u064d\u0660"+
		"\u0672\u0672\u06d8\u06f1\u06fc\u0701\u0711\u0711\u0713\u0713\u0732\u0781"+
		"\u07a8\u07b3\u07cc\u07f7\u07fc\u07fc\u0902\u0905\u093e\u093e\u0940\u0950"+
		"\u0953\u0957\u0964\u0965\u0983\u0985\u09be\u09c6\u09c9\u09ca\u09cd\u09cf"+
		"\u09d9\u09d9\u09e4\u09e5\u0a03\u0a05\u0a3e\u0a3e\17\2\u0abe\u0ad1\u0b84"+
		"\u0b84\u0bc0\u0bc4\u0bc8\u0bca\u0bcc\u0bcf\u0c03\u0c05\u0c40\u0c58\u0d40"+
		"\u0d46\u0d48\u0d4a\u0e32\u0e3c\u0e49\u0e50\u0eb3\u0ebe\u0eca\u0ecf\4\2"+
		"\62;\u07c2\u07cb\7\2..\60\60<<\u00af\u00af\u066d\u066e\7\2##..\60\60="+
		"=AA\n\2\13\f\16\17\"\"$$*+>>@@}\177\13\2\13\f\16\17\"$*+..\60\60>>@A}"+
		"\177\f\2\13\f\16\17\"$&&)+.\60>>@Aab}\177\n\2\13\f\16\17\"$*+.\60>>@A"+
		"}\177\n\2\13\f\16\17\"\"$$*+>>@@~~\5\2\62;C\\c|\13\2\13\f\16\17\"\"$$"+
		"*+>>@@}\177\u00a2\u00a2\r\2\13\f\16\17\"\"$$*+..\60\60>>@@}\177\u00a2"+
		"\u00a2\u0280\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2"+
		"\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2"+
		"\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2"+
		"\2\2\2#\3\2\2\2\3k\3\2\2\2\5s\3\2\2\2\7u\3\2\2\2\t|\3\2\2\2\13\u0080\3"+
		"\2\2\2\r\u0085\3\2\2\2\17\u0094\3\2\2\2\21\u00a3\3\2\2\2\23\u00aa\3\2"+
		"\2\2\25\u00c3\3\2\2\2\27\u00c5\3\2\2\2\31\u00d8\3\2\2\2\33\u00ff\3\2\2"+
		"\2\35\u0128\3\2\2\2\37\u013d\3\2\2\2!\u0142\3\2\2\2#\u0148\3\2\2\2%\u014a"+
		"\3\2\2\2\'\u014c\3\2\2\2)\u014e\3\2\2\2+\u0166\3\2\2\2-\u0169\3\2\2\2"+
		"/\u016d\3\2\2\2\61\u0184\3\2\2\2\63\u0186\3\2\2\2\65\u018a\3\2\2\2\67"+
		"\u0192\3\2\2\29\u019c\3\2\2\2;\u020e\3\2\2\2=\u0210\3\2\2\2?\u0212\3\2"+
		"\2\2A\u0214\3\2\2\2C\u0216\3\2\2\2E\u0218\3\2\2\2G\u021a\3\2\2\2I\u021d"+
		"\3\2\2\2K\u0226\3\2\2\2MN\7/\2\2NO\7N\2\2OP\7T\2\2PQ\7D\2\2Ql\7/\2\2R"+
		"S\7/\2\2ST\7T\2\2TU\7T\2\2UV\7D\2\2Vl\7/\2\2WX\7/\2\2XY\7N\2\2YZ\7E\2"+
		"\2Z[\7D\2\2[l\7/\2\2\\]\7/\2\2]^\7T\2\2^_\7E\2\2_`\7D\2\2`l\7/\2\2ab\7"+
		"/\2\2bc\7N\2\2cd\7U\2\2de\7D\2\2el\7/\2\2fg\7/\2\2gh\7T\2\2hi\7U\2\2i"+
		"j\7D\2\2jl\7/\2\2kM\3\2\2\2kR\3\2\2\2kW\3\2\2\2k\\\3\2\2\2ka\3\2\2\2k"+
		"f\3\2\2\2l\4\3\2\2\2mt\7$\2\2no\7b\2\2ot\7b\2\2pq\7)\2\2qt\7)\2\2rt\t"+
		"\2\2\2sm\3\2\2\2sn\3\2\2\2sp\3\2\2\2sr\3\2\2\2t\6\3\2\2\2uv\5\63\32\2"+
		"vw\t\3\2\2wx\5\63\32\2xy\t\3\2\2yz\5\65\33\2z\b\3\2\2\2{}\t\4\2\2|{\3"+
		"\2\2\2|}\3\2\2\2}~\3\2\2\2~\177\5\61\31\2\177\n\3\2\2\2\u0080\u0081\5"+
		"\67\34\2\u0081\u0082\t\5\2\2\u0082\u0083\5\67\34\2\u0083\f\3\2\2\2\u0084"+
		"\u0086\5/\30\2\u0085\u0084\3\2\2\2\u0086\u0087\3\2\2\2\u0087\u0085\3\2"+
		"\2\2\u0087\u0088\3\2\2\2\u0088\u0091\3\2\2\2\u0089\u008b\t\6\2\2\u008a"+
		"\u008c\5/\30\2\u008b\u008a\3\2\2\2\u008c\u008d\3\2\2\2\u008d\u008b\3\2"+
		"\2\2\u008d\u008e\3\2\2\2\u008e\u0090\3\2\2\2\u008f\u0089\3\2\2\2\u0090"+
		"\u0093\3\2\2\2\u0091\u008f\3\2\2\2\u0091\u0092\3\2\2\2\u0092\16\3\2\2"+
		"\2\u0093\u0091\3\2\2\2\u0094\u0098\7B\2\2\u0095\u0099\5%\23\2\u0096\u0099"+
		"\5\'\24\2\u0097\u0099\7a\2\2\u0098\u0095\3\2\2\2\u0098\u0096\3\2\2\2\u0098"+
		"\u0097\3\2\2\2\u0099\u00a0\3\2\2\2\u009a\u009f\5%\23\2\u009b\u009f\5\'"+
		"\24\2\u009c\u009f\7a\2\2\u009d\u009f\5-\27\2\u009e\u009a\3\2\2\2\u009e"+
		"\u009b\3\2\2\2\u009e\u009c\3\2\2\2\u009e\u009d\3\2\2\2\u009f\u00a2\3\2"+
		"\2\2\u00a0\u009e\3\2\2\2\u00a0\u00a1\3\2\2\2\u00a1\20\3\2\2\2\u00a2\u00a0"+
		"\3\2\2\2\u00a3\u00a5\7%\2\2\u00a4\u00a6\5+\26\2\u00a5\u00a4\3\2\2\2\u00a6"+
		"\u00a7\3\2\2\2\u00a7\u00a5\3\2\2\2\u00a7\u00a8\3\2\2\2\u00a8\22\3\2\2"+
		"\2\u00a9\u00ab\5/\30\2\u00aa\u00a9\3\2\2\2\u00ab\u00ac\3\2\2\2\u00ac\u00aa"+
		"\3\2\2\2\u00ac\u00ad\3\2\2\2\u00ad\u00b6\3\2\2\2\u00ae\u00b0\7\60\2\2"+
		"\u00af\u00b1\5/\30\2\u00b0\u00af\3\2\2\2\u00b1\u00b2\3\2\2\2\u00b2\u00b0"+
		"\3\2\2\2\u00b2\u00b3\3\2\2\2\u00b3\u00b5\3\2\2\2\u00b4\u00ae\3\2\2\2\u00b5"+
		"\u00b8\3\2\2\2\u00b6\u00b4\3\2\2\2\u00b6\u00b7\3\2\2\2\u00b7\u00b9\3\2"+
		"\2\2\u00b8\u00b6\3\2\2\2\u00b9\u00ba\7\60\2\2\u00ba\u00bb\5;\36\2\u00bb"+
		"\24\3\2\2\2\u00bc\u00bd\t\7\2\2\u00bd\u00be\7-\2\2\u00be\u00c4\7-\2\2"+
		"\u00bf\u00c0\t\7\2\2\u00c0\u00c4\7%\2\2\u00c1\u00c2\t\b\2\2\u00c2\u00c4"+
		"\7%\2\2\u00c3\u00bc\3\2\2\2\u00c3\u00bf\3\2\2\2\u00c3\u00c1\3\2\2\2\u00c4"+
		"\26\3\2\2\2\u00c5\u00c6\7j\2\2\u00c6\u00c7\7v\2\2\u00c7\u00c8\7v\2\2\u00c8"+
		"\u00c9\7r\2\2\u00c9\u00cb\3\2\2\2\u00ca\u00cc\7u\2\2\u00cb\u00ca\3\2\2"+
		"\2\u00cb\u00cc\3\2\2\2\u00cc\u00cd\3\2\2\2\u00cd\u00ce\7<\2\2\u00ce\u00cf"+
		"\7\61\2\2\u00cf\u00d0\7\61\2\2\u00d0\u00d2\3\2\2\2\u00d1\u00d3\5=\37\2"+
		"\u00d2\u00d1\3\2\2\2\u00d3\u00d4\3\2\2\2\u00d4\u00d2\3\2\2\2\u00d4\u00d5"+
		"\3\2\2\2\u00d5\u00d6\3\2\2\2\u00d6\u00d7\5C\"\2\u00d7\30\3\2\2\2\u00d8"+
		"\u00d9\7y\2\2\u00d9\u00da\7y\2\2\u00da\u00db\7y\2\2\u00db\u00dc\7\60\2"+
		"\2\u00dc\u00e4\3\2\2\2\u00dd\u00df\5? \2\u00de\u00dd\3\2\2\2\u00df\u00e0"+
		"\3\2\2\2\u00e0\u00de\3\2\2\2\u00e0\u00e1\3\2\2\2\u00e1\u00e2\3\2\2\2\u00e2"+
		"\u00e3\7\60\2\2\u00e3\u00e5\3\2\2\2\u00e4\u00de\3\2\2\2\u00e5\u00e6\3"+
		"\2\2\2\u00e6\u00e4\3\2\2\2\u00e6\u00e7\3\2\2\2\u00e7\u00ea\3\2\2\2\u00e8"+
		"\u00eb\5%\23\2\u00e9\u00eb\5\'\24\2\u00ea\u00e8\3\2\2\2\u00ea\u00e9\3"+
		"\2\2\2\u00eb\u00ee\3\2\2\2\u00ec\u00ef\5%\23\2\u00ed\u00ef\5\'\24\2\u00ee"+
		"\u00ec\3\2\2\2\u00ee\u00ed\3\2\2\2\u00ef\u00f2\3\2\2\2\u00f0\u00f3\5%"+
		"\23\2\u00f1\u00f3\5\'\24\2\u00f2\u00f0\3\2\2\2\u00f2\u00f1\3\2\2\2\u00f2"+
		"\u00f3\3\2\2\2\u00f3\u00f6\3\2\2\2\u00f4\u00f7\5%\23\2\u00f5\u00f7\5\'"+
		"\24\2\u00f6\u00f4\3\2\2\2\u00f6\u00f5\3\2\2\2\u00f6\u00f7\3\2\2\2\u00f7"+
		"\32\3\2\2\2\u00f8\u00fa\5A!\2\u00f9\u00f8\3\2\2\2\u00fa\u00fb\3\2\2\2"+
		"\u00fb\u00f9\3\2\2\2\u00fb\u00fc\3\2\2\2\u00fc\u00fd\3\2\2\2\u00fd\u00fe"+
		"\7\60\2\2\u00fe\u0100\3\2\2\2\u00ff\u00f9\3\2\2\2\u0100\u0101\3\2\2\2"+
		"\u0101\u00ff\3\2\2\2\u0101\u0102\3\2\2\2\u0102\u0117\3\2\2\2\u0103\u0104"+
		"\7e\2\2\u0104\u0105\7q\2\2\u0105\u0118\7o\2\2\u0106\u0107\7p\2\2\u0107"+
		"\u0108\7g\2\2\u0108\u0118\7v\2\2\u0109\u010a\7q\2\2\u010a\u010b\7t\2\2"+
		"\u010b\u0118\7i\2\2\u010c\u010d\7g\2\2\u010d\u010e\7f\2\2\u010e\u0118"+
		"\7w\2\2\u010f\u0110\7p\2\2\u0110\u0111\7c\2\2\u0111\u0112\7o\2\2\u0112"+
		"\u0118\7g\2\2\u0113\u0114\7k\2\2\u0114\u0115\7p\2\2\u0115\u0116\7h\2\2"+
		"\u0116\u0118\7q\2\2\u0117\u0103\3\2\2\2\u0117\u0106\3\2\2\2\u0117\u0109"+
		"\3\2\2\2\u0117\u010c\3\2\2\2\u0117\u010f\3\2\2\2\u0117\u0113\3\2\2\2\u0118"+
		"\u0121\3\2\2\2\u0119\u011b\7\61\2\2\u011a\u011c\5E#\2\u011b\u011a\3\2"+
		"\2\2\u011c\u011d\3\2\2\2\u011d\u011b\3\2\2\2\u011d\u011e\3\2\2\2\u011e"+
		"\u011f\3\2\2\2\u011f\u0120\5G$\2\u0120\u0122\3\2\2\2\u0121\u0119\3\2\2"+
		"\2\u0121\u0122\3\2\2\2\u0122\34\3\2\2\2\u0123\u0124\7(\2\2\u0124\u0125"+
		"\7n\2\2\u0125\u0126\7v\2\2\u0126\u0129\7=\2\2\u0127\u0129\7>\2\2\u0128"+
		"\u0123\3\2\2\2\u0128\u0127\3\2\2\2\u0128\u0129\3\2\2\2\u0129\u012a\3\2"+
		"\2\2\u012a\u012b\5I%\2\u012b\u0131\7B\2\2\u012c\u012d\5K&\2\u012d\u012e"+
		"\7\60\2\2\u012e\u0130\3\2\2\2\u012f\u012c\3\2\2\2\u0130\u0133\3\2\2\2"+
		"\u0131\u012f\3\2\2\2\u0131\u0132\3\2\2\2\u0132\u0134\3\2\2\2\u0133\u0131"+
		"\3\2\2\2\u0134\u013a\5K&\2\u0135\u0136\7(\2\2\u0136\u0137\7i\2\2\u0137"+
		"\u0138\7v\2\2\u0138\u013b\7=\2\2\u0139\u013b\7@\2\2\u013a\u0135\3\2\2"+
		"\2\u013a\u0139\3\2\2\2\u013a\u013b\3\2\2\2\u013b\36\3\2\2\2\u013c\u013e"+
		"\59\35\2\u013d\u013c\3\2\2\2\u013e\u013f\3\2\2\2\u013f\u013d\3\2\2\2\u013f"+
		"\u0140\3\2\2\2\u0140 \3\2\2\2\u0141\u0143\t\t\2\2\u0142\u0141\3\2\2\2"+
		"\u0143\u0144\3\2\2\2\u0144\u0142\3\2\2\2\u0144\u0145\3\2\2\2\u0145\u0146"+
		"\3\2\2\2\u0146\u0147\b\21\2\2\u0147\"\3\2\2\2\u0148\u0149\13\2\2\2\u0149"+
		"$\3\2\2\2\u014a\u014b\4c|\2\u014b&\3\2\2\2\u014c\u014d\4C\\\2\u014d(\3"+
		"\2\2\2\u014e\u014f\7(\2\2\u014f\u015d\t\n\2\2\u0150\u0151\7c\2\2\u0151"+
		"\u0152\7e\2\2\u0152\u0153\7w\2\2\u0153\u0154\7v\2\2\u0154\u015e\7g\2\2"+
		"\u0155\u0156\7i\2\2\u0156\u0157\7t\2\2\u0157\u0158\7c\2\2\u0158\u0159"+
		"\7x\2\2\u0159\u015e\7g\2\2\u015a\u015b\7w\2\2\u015b\u015c\7o\2\2\u015c"+
		"\u015e\7n\2\2\u015d\u0150\3\2\2\2\u015d\u0155\3\2\2\2\u015d\u015a\3\2"+
		"\2\2\u015e*\3\2\2\2\u015f\u0167\5%\23\2\u0160\u0167\5\'\24\2\u0161\u0167"+
		"\5)\25\2\u0162\u0167\t\13\2\2\u0163\u0164\4\u0a40\u0a51\2\u0164\u0167"+
		"\4\u0a83\u0a85\2\u0165\u0167\t\f\2\2\u0166\u015f\3\2\2\2\u0166\u0160\3"+
		"\2\2\2\u0166\u0161\3\2\2\2\u0166\u0162\3\2\2\2\u0166\u0163\3\2\2\2\u0166"+
		"\u0165\3\2\2\2\u0167,\3\2\2\2\u0168\u016a\t\r\2\2\u0169\u0168\3\2\2\2"+
		"\u016a.\3\2\2\2\u016b\u016e\5+\26\2\u016c\u016e\5-\27\2\u016d\u016b\3"+
		"\2\2\2\u016d\u016c\3\2\2\2\u016e\60\3\2\2\2\u016f\u0171\5-\27\2\u0170"+
		"\u016f\3\2\2\2\u0171\u0172\3\2\2\2\u0172\u0170\3\2\2\2\u0172\u0173\3\2"+
		"\2\2\u0173\u0185\3\2\2\2\u0174\u0176\5-\27\2\u0175\u0174\3\2\2\2\u0176"+
		"\u0179\3\2\2\2\u0177\u0175\3\2\2\2\u0177\u0178\3\2\2\2\u0178\u0180\3\2"+
		"\2\2\u0179\u0177\3\2\2\2\u017a\u017c\t\16\2\2\u017b\u017d\5-\27\2\u017c"+
		"\u017b\3\2\2\2\u017d\u017e\3\2\2\2\u017e\u017c\3\2\2\2\u017e\u017f\3\2"+
		"\2\2\u017f\u0181\3\2\2\2\u0180\u017a\3\2\2\2\u0181\u0182\3\2\2\2\u0182"+
		"\u0180\3\2\2\2\u0182\u0183\3\2\2\2\u0183\u0185\3\2\2\2\u0184\u0170\3\2"+
		"\2\2\u0184\u0177\3\2\2\2\u0185\62\3\2\2\2\u0186\u0188\5-\27\2\u0187\u0189"+
		"\5-\27\2\u0188\u0187\3\2\2\2\u0188\u0189\3\2\2\2\u0189\64\3\2\2\2\u018a"+
		"\u018b\5-\27\2\u018b\u018d\5-\27\2\u018c\u018e\5-\27\2\u018d\u018c\3\2"+
		"\2\2\u018d\u018e\3\2\2\2\u018e\u0190\3\2\2\2\u018f\u0191\5-\27\2\u0190"+
		"\u018f\3\2\2\2\u0190\u0191\3\2\2\2\u0191\66\3\2\2\2\u0192\u0194\5-\27"+
		"\2\u0193\u0195\5-\27\2\u0194\u0193\3\2\2\2\u0194\u0195\3\2\2\2\u0195\u0197"+
		"\3\2\2\2\u0196\u0198\5-\27\2\u0197\u0196\3\2\2\2\u0197\u0198\3\2\2\2\u0198"+
		"\u019a\3\2\2\2\u0199\u019b\5-\27\2\u019a\u0199\3\2\2\2\u019a\u019b\3\2"+
		"\2\2\u019b8\3\2\2\2\u019c\u019d\t\17\2\2\u019d:\3\2\2\2\u019e\u019f\7"+
		"d\2\2\u019f\u01a0\7c\2\2\u01a0\u020f\7v\2\2\u01a1\u01a2\7d\2\2\u01a2\u01a3"+
		"\7o\2\2\u01a3\u020f\7r\2\2\u01a4\u020f\7e\2\2\u01a5\u01a6\7e\2\2\u01a6"+
		"\u01a7\7n\2\2\u01a7\u01a8\7c\2\2\u01a8\u01a9\7u\2\2\u01a9\u020f\7u\2\2"+
		"\u01aa\u01ab\7e\2\2\u01ab\u01ac\7i\2\2\u01ac\u020f\7k\2\2\u01ad\u01ae"+
		"\7e\2\2\u01ae\u01af\7r\2\2\u01af\u020f\7r\2\2\u01b0\u01b1\7f\2\2\u01b1"+
		"\u01b2\7n\2\2\u01b2\u020f\7n\2\2\u01b3\u01b4\7f\2\2\u01b4\u01b5\7q\2\2"+
		"\u01b5\u020f\7e\2\2\u01b6\u01b7\7f\2\2\u01b7\u01b8\7q\2\2\u01b8\u01b9"+
		"\7e\2\2\u01b9\u020f\7z\2\2\u01ba\u01bb\7g\2\2\u01bb\u01bc\7z\2\2\u01bc"+
		"\u020f\7g\2\2\u01bd\u01be\7i\2\2\u01be\u01bf\7k\2\2\u01bf\u020f\7h\2\2"+
		"\u01c0\u01c1\7i\2\2\u01c1\u020f\7|\2\2\u01c2\u020f\7j\2\2\u01c3\u01c4"+
		"\7j\2\2\u01c4\u01c5\7v\2\2\u01c5\u020f\7o\2\2\u01c6\u01c7\7j\2\2\u01c7"+
		"\u01c8\7v\2\2\u01c8\u01c9\7o\2\2\u01c9\u020f\7n\2\2\u01ca\u01cb\7l\2\2"+
		"\u01cb\u01cc\7c\2\2\u01cc\u020f\7t\2\2\u01cd\u01ce\7l\2\2\u01ce\u01cf"+
		"\7c\2\2\u01cf\u01d0\7x\2\2\u01d0\u020f\7c\2\2\u01d1\u01d2\7l\2\2\u01d2"+
		"\u01d3\7r\2\2\u01d3\u01d4\7g\2\2\u01d4\u020f\7i\2\2\u01d5\u01d6\7l\2\2"+
		"\u01d6\u01d7\7r\2\2\u01d7\u020f\7i\2\2\u01d8\u01d9\7o\2\2\u01d9\u01da"+
		"\7q\2\2\u01da\u020f\7x\2\2\u01db\u01dc\7o\2\2\u01dc\u01dd\7r\2\2\u01dd"+
		"\u020f\7\65\2\2\u01de\u01df\7r\2\2\u01df\u01e0\7f\2\2\u01e0\u020f\7h\2"+
		"\2\u01e1\u01e2\7r\2\2\u01e2\u01e3\7j\2\2\u01e3\u020f\7r\2\2\u01e4\u01e5"+
		"\7r\2\2\u01e5\u020f\7n\2\2\u01e6\u01e7\7r\2\2\u01e7\u01e8\7p\2\2\u01e8"+
		"\u020f\7i\2\2\u01e9\u01ea\7r\2\2\u01ea\u01eb\7r\2\2\u01eb\u020f\7v\2\2"+
		"\u01ec\u01ed\7r\2\2\u01ed\u01ee\7r\2\2\u01ee\u01ef\7v\2\2\u01ef\u020f"+
		"\7z\2\2\u01f0\u01f1\7r\2\2\u01f1\u020f\7u\2\2\u01f2\u01f3\7r\2\2\u01f3"+
		"\u020f\7{\2\2\u01f4\u01f5\7u\2\2\u01f5\u01f6\7e\2\2\u01f6\u01f7\7c\2\2"+
		"\u01f7\u01f8\7n\2\2\u01f8\u020f\7c\2\2\u01f9\u01fa\7u\2\2\u01fa\u01fb"+
		"\7s\2\2\u01fb\u020f\7n\2\2\u01fc\u01fd\7v\2\2\u01fd\u01fe\7c\2\2\u01fe"+
		"\u020f\7t\2\2\u01ff\u0200\7v\2\2\u0200\u0201\7i\2\2\u0201\u020f\7|\2\2"+
		"\u0202\u0203\7v\2\2\u0203\u0204\7z\2\2\u0204\u020f\7v\2\2\u0205\u0206"+
		"\7y\2\2\u0206\u0207\7c\2\2\u0207\u020f\7x\2\2\u0208\u0209\7z\2\2\u0209"+
		"\u020a\7o\2\2\u020a\u020f\7n\2\2\u020b\u020c\7|\2\2\u020c\u020d\7k\2\2"+
		"\u020d\u020f\7r\2\2\u020e\u019e\3\2\2\2\u020e\u01a1\3\2\2\2\u020e\u01a4"+
		"\3\2\2\2\u020e\u01a5\3\2\2\2\u020e\u01aa\3\2\2\2\u020e\u01ad\3\2\2\2\u020e"+
		"\u01b0\3\2\2\2\u020e\u01b3\3\2\2\2\u020e\u01b6\3\2\2\2\u020e\u01ba\3\2"+
		"\2\2\u020e\u01bd\3\2\2\2\u020e\u01c0\3\2\2\2\u020e\u01c2\3\2\2\2\u020e"+
		"\u01c3\3\2\2\2\u020e\u01c6\3\2\2\2\u020e\u01ca\3\2\2\2\u020e\u01cd\3\2"+
		"\2\2\u020e\u01d1\3\2\2\2\u020e\u01d5\3\2\2\2\u020e\u01d8\3\2\2\2\u020e"+
		"\u01db\3\2\2\2\u020e\u01de\3\2\2\2\u020e\u01e1\3\2\2\2\u020e\u01e4\3\2"+
		"\2\2\u020e\u01e6\3\2\2\2\u020e\u01e9\3\2\2\2\u020e\u01ec\3\2\2\2\u020e"+
		"\u01f0\3\2\2\2\u020e\u01f2\3\2\2\2\u020e\u01f4\3\2\2\2\u020e\u01f9\3\2"+
		"\2\2\u020e\u01fc\3\2\2\2\u020e\u01ff\3\2\2\2\u020e\u0202\3\2\2\2\u020e"+
		"\u0205\3\2\2\2\u020e\u0208\3\2\2\2\u020e\u020b\3\2\2\2\u020f<\3\2\2\2"+
		"\u0210\u0211\n\20\2\2\u0211>\3\2\2\2\u0212\u0213\n\21\2\2\u0213@\3\2\2"+
		"\2\u0214\u0215\n\22\2\2\u0215B\3\2\2\2\u0216\u0217\n\23\2\2\u0217D\3\2"+
		"\2\2\u0218\u0219\n\24\2\2\u0219F\3\2\2\2\u021a\u021b\n\23\2\2\u021bH\3"+
		"\2\2\2\u021c\u021e\t\25\2\2\u021d\u021c\3\2\2\2\u021e\u0222\3\2\2\2\u021f"+
		"\u0221\n\26\2\2\u0220\u021f\3\2\2\2\u0221\u0224\3\2\2\2\u0222\u0220\3"+
		"\2\2\2\u0222\u0223\3\2\2\2\u0223J\3\2\2\2\u0224\u0222\3\2\2\2\u0225\u0227"+
		"\n\27\2\2\u0226\u0225\3\2\2\2\u0227\u0228\3\2\2\2\u0228\u0226\3\2\2\2"+
		"\u0228\u0229\3\2\2\2\u0229L\3\2\2\2\67\2ks|\u0087\u008d\u0091\u0098\u009e"+
		"\u00a0\u00a7\u00ac\u00b2\u00b6\u00c3\u00cb\u00d4\u00e0\u00e6\u00ea\u00ee"+
		"\u00f2\u00f6\u00fb\u0101\u0117\u011d\u0121\u0128\u0131\u013a\u013f\u0142"+
		"\u0144\u015d\u0166\u0169\u016d\u0172\u0177\u017e\u0182\u0184\u0188\u018d"+
		"\u0190\u0194\u0197\u019a\u020e\u021d\u0222\u0228\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}