/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.charamel.xml.feedback.action;

import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.xtension.charamel.CharamelExecutor;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author Manuel
 */
public abstract class CharaXMLElement {
    
    List<CharaXMLElement> children = new ArrayList();
    CharaXMLElement parent;
    String text;
    
    CharaXMLElement(){}
    
    CharaXMLElement(CharaXMLElement parent){
        this.parent = parent;
    }
    
    public List<CharaXMLElement> getChildren(){
        return children;
    }
    
    public boolean hasChildren(){
        return children.isEmpty();
    }
    
    public void handle(){
        children.stream().forEach((c) -> {c.handle();});
    }
    
    public void parseXML(){}
        
    void parseChildren(Element element){
        NodeList list = element.getChildNodes();
        List<Element> childElementList = new ArrayList(); 
        for (int i = 0; i < list.getLength(); i++) {
            if (list.item(i).getNodeType() == Node.ELEMENT_NODE) {
                childElementList.add((Element) list.item(i));  
            }
            else if (list.item(i).getNodeType() == Node.TEXT_NODE) {
                text=list.item(i).getTextContent();
            }
            
        }
        CharaXMLElement child;
        for (Element childElement : childElementList){
            String name = childElement.getTagName();
            switch (name){
                case "cai_event":
                    child = new CaiEvent(this);           
                    try {
                        ((CaiEvent)child).parseXML(childElement);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing cai_event", ex);
                    }
                    children.add(child);
                    break;
                case "tts":
                    child = new Tts(this);           
                    try {
                        ((Tts)child).parseXML(childElement);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing tts", ex);
                    }
                    children.add(child);
                    break;
                case "action":
                    child = new Action(this);           
                    try {
                        ((Action)child).parseXML(childElement);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing action", ex);
                    }
                    children.add(child);
                    break;
                case "status":
                    child = new Status(this);           
                    try {
                        ((Status)child).parseXML(childElement);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing action", ex);
                    }
                    children.add(child);
                    break;
                case "cai_command":
                    child = new CaiCommand(this);           
                    try {
                        ((CaiCommand)child).parseXML(childElement);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing action", ex);
                    }
                    children.add(child);
                    break;   
                case "cai_response":
                    child = new CaiResponse(this);           
                    try {
                        ((CaiResponse)child).parseXML(childElement);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing action", ex);
                    }
                    children.add(child);
                    break;    
                    
                default:
                    Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "Could not parse XML tag:{0}", name);
                   
                /*case "feedback":
                    child = new Feedback(this);           
                    try {
                        ((Feedback)child).parseXML(childElement);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing feedback", ex);
                    }
                    children.add(child);
                    break;
                */
            }
        }
    }

    public void handle(CharamelExecutor executor) {}
    
    
}
