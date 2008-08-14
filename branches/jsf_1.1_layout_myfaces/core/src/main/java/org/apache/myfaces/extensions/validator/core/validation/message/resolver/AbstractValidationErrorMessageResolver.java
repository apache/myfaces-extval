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
package org.apache.myfaces.extensions.validator.core.validation.message.resolver;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ELUtils;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * @author Gerhard Petracek
 */
public abstract class AbstractValidationErrorMessageResolver implements
        MessageResolver
{
    private static String deactivateDefaultConvention = WebXmlParameter.DEACTIVATE_DEFAULT_CONVENTION;
    private static ResourceBundle defaultBundle = null;
    private String messageBundleBaseName;
    //with jsf 1.1 only available if there is a custom bean
    private String messageBundleVarName;

    protected final Log logger = LogFactory.getLog(getClass());

    public String getMessage(String key, Locale locale)
    {
        if (key == null || key.equals(""))
        {
            return null;
        }

        ResourceBundle resourceBundle = null;
        String customMessage = null;

        //only in case of a ValidationErrorMessageResolver which is configured as bean
        if (this.messageBundleBaseName != null)
        {
            resourceBundle = ResourceBundle.getBundle(
                    this.messageBundleBaseName, locale);
            if (resourceBundle != null)
            {
                customMessage = resourceBundle.getString(key);
            }
            else
            {
                logger.warn("message bundle " + this.messageBundleBaseName
                        + " not found");
            }
        }

        //only in case of a ValidationErrorMessageResolver which is configured as bean
        if (this.messageBundleVarName != null && customMessage == null)
        {
            resourceBundle = (ResourceBundle) ELUtils
                    .getBean(messageBundleVarName);
            if (resourceBundle != null)
            {
                customMessage = resourceBundle.getString(key);
            }
            else
            {
                logger.warn("message bundle var name "
                        + this.messageBundleVarName + " not found");
            }
        }

        if (customMessage != null)
        {
            return customMessage;
        }

        /*
         * try to use the convention for the message bundle
         */
        customMessage = tryToUseMessageBundleConvention(key, locale);

        if (customMessage != null)
        {
            return customMessage;
        }

        /*
         * no message bundle or message found (with the convention)?
         */

        //try to load custom messages
        try
        {
            resourceBundle = ResourceBundle.getBundle(getCustomBaseName(),
                    locale);
        }
        catch (Throwable t)
        {
            //do nothing - it was just a try
        }

        if (resourceBundle != null)
        {
            try
            {
                customMessage = resourceBundle.getString(key);
            }
            catch (MissingResourceException e)
            {
                logger.trace("no custom message for " + key + " within "
                        + getCustomBaseName());
            }
        }

        //use custom name (if possible) otherwise: fallback to default message (if possible)
        return (customMessage != null) ? customMessage
                : (getBaseName() != null) ? ResourceBundle.getBundle(
                        getBaseName(), locale).getString(key) : null;
    }

    private String tryToUseMessageBundleConvention(String key, Locale locale)
    {
        String customMessage = null;

        if ((deactivateDefaultConvention == null || !deactivateDefaultConvention
                .equalsIgnoreCase("true"))
                && isDefaultMessageBundleConventionActive())
        {
            if (defaultBundle == null)
            {
                try
                {
                    defaultBundle = ResourceBundle.getBundle(ExtValUtils
                            .getInformationProviderBean()
                            .getConventionForMessageBundle(), locale);
                }
                catch (Throwable t)
                {
                    //do nothing
                    deactivateDefaultConvention = "true";
                }
            }

            if (defaultBundle != null)
            {
                try
                {
                    customMessage = defaultBundle.getString(key);
                }
                catch (MissingResourceException e)
                {
                    //do nothing
                }
            }
        }

        return customMessage;
    }

    protected boolean isDefaultMessageBundleConventionActive()
    {
        return true;
    }

    protected abstract String getBaseName();

    protected String getCustomBaseName()
    {
        return null;
    }

    public void setMessageBundleBaseName(String messageBundleBaseName)
    {
        this.messageBundleBaseName = messageBundleBaseName;
    }

    public void setMessageBundleVarName(String messageBundleVarName)
    {
        this.messageBundleVarName = messageBundleVarName;
    }
}
