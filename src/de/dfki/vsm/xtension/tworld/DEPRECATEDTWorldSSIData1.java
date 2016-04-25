package de.dfki.vsm.xtension.tworld;

import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLUtilities;
import java.util.HashMap;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class DEPRECATEDTWorldSSIData1 implements XMLParseable {

    public final class VoiceData implements XMLParseable {

        public final class PraatData implements XMLParseable {

            private String mPitchMean;
            private String mPitchSD;
            private String mSpeechRate;
            private String mIntensity;

            public String getPitchSD() {
                return mPitchSD;
            }

            public String getPitchMean() {
                return mPitchMean;
            }

            public String getSpeechRate() {
                return mSpeechRate;
            }

            public String getIntensity() {
                return mIntensity;
            }

            @Override
            public final void parseXML(final Element element) throws XMLParseError {
                if (element.getTagName().equals("praat")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            final String tag = element.getTagName();
                            if (tag.equals("PitchMean")) {
                                mPitchMean = element.getTextContent();
                            } else if (tag.equals("PitchSD")) {
                                mPitchSD = element.getTextContent();
                            } else if (tag.equals("SpeechRate")) {
                                mSpeechRate = element.getTextContent();
                            } else if (tag.equals("Intensity")) {
                                mIntensity = element.getTextContent();
                            } else {
                                // Do nothing
                            }
                        }
                    });
                }
            }
        }

        private String mActivity = new String();
        private String mKeyword = new String();
        private PraatData mPraatData = new PraatData();

        public String getActivity() {
            return mActivity;
        }

        public String getKeyword() {
            return mKeyword;
        }

        public final PraatData getPraatData() {
            return mPraatData;
        }

        @Override
        public final void parseXML(final Element element) throws XMLParseError {
            if (element.getTagName().equals("voice")) {
                XMLParseAction.processChildNodes(element, new XMLParseAction() {
                    @Override
                    public void run(final Element element) throws XMLParseError {
                        final String tag = element.getTagName();

                        if (tag.equals("praat")) {
                            mPraatData = new PraatData();
                            mPraatData.parseXML(element);
                        } else if (tag.equals("activity")) {
                            mActivity = element.getTextContent();
                        } else if (tag.equals("keyword")) {
                            mKeyword = element.getTextContent();
                        } else {
                            // Do nothing
                        }
                    }
                });
            }
        }
    }

    public final class HeadData implements XMLParseable {

        public final class PosData implements XMLParseable {

            private String mX;
            private String mY;

            public String getX() {
                return mX;
            }

            public String getY() {
                return mY;
            }

            @Override
            public final void parseXML(final Element element) throws XMLParseError {
                if (element.getTagName().equals("pos")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            final String tag = element.getTagName();
                            if (tag.equals("x")) {
                                mX = element.getTextContent();
                            } else if (tag.equals("y")) {
                                mY = element.getTextContent();
                            } else {
                                // Do nothing
                            }
                        }
                    });
                }
            }
        }

        private String mNod;
        private String mShake;
        private PosData mPosData;

        public String getNod() {
            return mNod;
        }

        public String getShake() {
            return mShake;
        }

        public PosData getPosData() {
            return mPosData;
        }

        @Override
        public final void parseXML(final Element element) throws XMLParseError {
            if (element.getTagName().equals("head")) {
                XMLParseAction.processChildNodes(element, new XMLParseAction() {
                    @Override
                    public void run(final Element element) throws XMLParseError {
                        final String tag = element.getTagName();
                        if (tag.equals("pos")) {
                            mPosData = new PosData();
                            mPosData.parseXML(element);
                        } else if (tag.equals("nod")) {
                            mNod = element.getTextContent();
                        } else if (tag.equals("shake")) {
                            mShake = element.getTextContent();
                        } else {
                            // Do nothing
                        }
                    }
                });
            }
        }
    }

    public final class BodyData implements XMLParseable {

        public final class LeanData implements XMLParseable {

            private String mIdentifier;
            private String mDetected;
            private String mDuration;
            private String mIntensity;

            public String getIdentifier() {
                return mIdentifier;
            }

            public String getDetected() {
                return mDetected;
            }

            public String getDuration() {
                return mDuration;
            }

            public String getIntensity() {
                return mIntensity;
            }

            @Override
            public final void parseXML(final Element element) throws XMLParseError {
                if (element.getTagName().equals("lean")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            final String tag = element.getTagName();
                            if (tag.equals("identifier")) {
                                mIdentifier = element.getTextContent();
                            } else if (tag.equals("detected")) {
                                mDetected = element.getTextContent();
                            } else if (tag.equals("duration")) {
                                mDuration = element.getTextContent();
                            } else if (tag.equals("intensity")) {
                                mIntensity = element.getTextContent();
                            } else {
                                // Do nothing
                            }
                        }
                    });
                }
            }
        }

        public final class GestData implements XMLParseable {

            private String mIdentifier;
            private String mDetected;
            private String mDuration;
            private String mIntensity;

            public String getIdentifier() {
                return mIdentifier;
            }

            public String getDetected() {
                return mDetected;
            }

            public String getDuration() {
                return mDuration;
            }

            public String getIntensity() {
                return mIntensity;
            }

            @Override
            public final void parseXML(final Element element) throws XMLParseError {
                if (element.getTagName().equals("gest")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            final String tag = element.getTagName();
                            if (tag.equals("identifier")) {
                                mIdentifier = element.getTextContent();
                            } else if (tag.equals("detected")) {
                                mDetected = element.getTextContent();
                            } else if (tag.equals("duration")) {
                                mDuration = element.getTextContent();
                            } else if (tag.equals("intensity")) {
                                mIntensity = element.getTextContent();
                            } else {
                                // Do nothing
                            }
                        }
                    });
                }
            }
        }

        private String mActivity;
        private String mOpeness;
        private String mEnergy;
        final HashMap<String, LeanData> mLeanData = new HashMap();
        final HashMap<String, GestData> mGestData = new HashMap();

        public String getActivity() {
            return mActivity;
        }

        public String getOpeness() {
            return mOpeness;
        }

        public String getEnergy() {
            return mEnergy;
        }

        public final LeanData getLeanData(final String name) {
            return mLeanData.get(name);
        }

        public final GestData getGestData(final String name) {
            return mGestData.get(name);
        }

        @Override
        public final void parseXML(final Element element) throws XMLParseError {
            if (element.getTagName().equals("body")) {
                XMLParseAction.processChildNodes(element, new XMLParseAction() {
                    @Override
                    public void run(final Element element) throws XMLParseError {
                        final String tag = element.getTagName();
                        if (tag.equals("activity")) {
                            mActivity = element.getTextContent();
                        } else if (tag.equals("openness")) {
                            mOpeness = element.getTextContent();
                        } else if (tag.equals("energy")) {
                            mEnergy = element.getTextContent();
                        } else if (tag.equals("leans")) {
                            XMLParseAction.processChildNodes(element, new XMLParseAction() {
                                @Override
                                public void run(final Element element) throws XMLParseError {
                                    final String tag = element.getTagName();
                                    if (tag.equals("lean")) {
                                        final LeanData data = new LeanData();
                                        data.parseXML(element);
                                        mLeanData.put(data.mIdentifier, data);
                                    } else {
                                        // Do nothing
                                    }
                                }
                            });
                        } else if (tag.equals("gests")) {
                            XMLParseAction.processChildNodes(element, new XMLParseAction() {
                                @Override
                                public void run(final Element element) throws XMLParseError {
                                    final String tag = element.getTagName();
                                    if (tag.equals("gest")) {
                                        final GestData data = new GestData();
                                        data.parseXML(element);
                                        mGestData.put(data.mIdentifier, data);
                                    } else {
                                        // Do nothing
                                    }
                                }
                            });
                        } else {
                            // Do nothing
                        }
                    }
                });
            }
        }
    }

    public final class FaceData implements XMLParseable {

        public final class ExpData implements XMLParseable {

            private String mIdentifier;
            private String mDetected;
            private String mDuration;
            private String mIntensity;

            public String getIdentifier() {
                return mIdentifier;
            }

            public String getDetected() {
                return mDetected;
            }

            public String getDuration() {
                return mDuration;
            }

            public String getIntensity() {
                return mIntensity;
            }

            @Override
            public final void parseXML(final Element element) throws XMLParseError {
                if (element.getTagName().equals("exp")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            final String tag = element.getTagName();
                            if (tag.equals("identifier")) {
                                mIdentifier = element.getTextContent();
                            } else if (tag.equals("detected")) {
                                mDetected = element.getTextContent();
                            } else if (tag.equals("duration")) {
                                mDuration = element.getTextContent();
                            } else if (tag.equals("intensity")) {
                                mIntensity = element.getTextContent();
                            } else {
                                // Do nothing
                            }
                        }
                    });
                }
            }
        }

        final HashMap<String, ExpData> mExpData = new HashMap();

        public final ExpData getExpData(final String name) {
            return mExpData.get(name);
        }

        @Override
        public final void parseXML(final Element element) throws XMLParseError {
            if (element.getTagName().equals("face")) {
                XMLParseAction.processChildNodes(element, new XMLParseAction() {
                    @Override
                    public void run(final Element element) throws XMLParseError {
                        final String tag = element.getTagName();
                        if (tag.equals("exps")) {
                            XMLParseAction.processChildNodes(element, new XMLParseAction() {
                                @Override
                                public void run(final Element element) throws XMLParseError {
                                    final String tag = element.getTagName();
                                    if (tag.equals("exp")) {
                                        final ExpData expData = new ExpData();
                                        expData.parseXML(element);
                                        mExpData.put(expData.mIdentifier, expData);
                                    } else {
                                        // Do nothing
                                    }
                                }
                            });
                        } else {
                            // Do nothing
                        }
                    }
                });
            }
        }
    }

    private VoiceData mVoiceData;
    private HeadData mHeadData;
    private BodyData mBodyData;
    private FaceData mFaceData;

    public final VoiceData getVoiceData() {
        return mVoiceData;
    }

    public final HeadData getHeadData() {
        return mHeadData;
    }

    public final BodyData getBodyData() {
        return mBodyData;
    }

    public final FaceData getFaceData() {
        return mFaceData;
    }

    public DEPRECATEDTWorldSSIData1(final String xml) {
        parse(xml);
    }

    private void parse(final String xml) {
        XMLUtilities.parseFromXMLString(this, xml, "UTF-8");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        if (element.getTagName().equals("ssi")) {
            XMLParseAction.processChildNodes(element, new XMLParseAction() {
                @Override
                public void run(final Element element) throws XMLParseError {
                    final String tag = element.getTagName();
                    if (tag.equals("voice")) {
                        mVoiceData = new VoiceData();
                        mVoiceData.parseXML(element);
                    } else if (tag.equals("head")) {
                        mHeadData = new HeadData();
                        mHeadData.parseXML(element);
                    } else if (tag.equals("body")) {
                        mBodyData = new BodyData();
                        mBodyData.parseXML(element);
                    } else if (tag.equals("face")) {
                        mFaceData = new FaceData();
                        mFaceData.parseXML(element);
                    } else {
                        // Do nothing
                    }
                }
            });
        }
    }
}
