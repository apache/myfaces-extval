/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.context.FacesContext;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class WebXmlUtils
{
    
    /**
     * Gets the initialization parameter from WEB.XML using the default prefix and removal of the spaces.
     * 
     * @param key the key name of the parameter
     * 
     * @return the value of the initialization parameter, if specified.
     */
    public static String getInitParameter(String key)
    {
        return getInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX, key, false);
    }

    /**
     * Gets the initialization parameter from WEB.XML using the specified prefix and removal of the spaces.
     * 
     * @param prefix the prefix of the parameter
     * @param name the name
     * 
     * @return the value of the initialization parameter, if specified.
     */
    public static String getInitParameter(String prefix, String name)
    {
        return getInitParameter(prefix, name, false);
    }
    
    /**
     * Gets the initialization parameter from WEB.XML using the default prefix and removal of spaces can be specified.
     * 
     * @param key the key name of the parameter
     * @param preserveBlanks should blanks be kept?
     * 
     * @return the value of the initialization parameter, if specified.
     */
    public static String getInitParameter(String key, boolean preserveBlanks)
    {
        return getInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX, key,
                preserveBlanks);
    }
    
    /**
     * Gets the initialization parameter from WEB.XML using the specified prefix and removal of spaces can be specified.
     * 
     * @param prefix the prefix of the parameter
     * @param key the key name of the parameter
     * @param preserveBlanks should blanks be kept?
     * 
     * @return the value of the initialization parameter, if specified.
     */
    public static String getInitParameter(String prefix, String key, boolean preserveBlanks)
    {
        String parameterName = key;
        if(prefix != null)
        {
            parameterName = prefix + "." + key;
        }
        String value = FacesContext.getCurrentInstance().getExternalContext().getInitParameter(parameterName);
        if (preserveBlanks)
        {
            return value;
        }
        else
        {
            return (value != null) ? value.replace(" ", "").trim() : null;
        }
        
    }
    
}
