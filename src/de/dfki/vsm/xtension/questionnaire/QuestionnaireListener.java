/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.questionnaire;

import java.util.HashMap;

/**
 *
 * @author Patrick Gebhard
 */
public interface QuestionnaireListener {
    
    public void  updateOnUestionnaire(HashMap<String, String> values);
}
