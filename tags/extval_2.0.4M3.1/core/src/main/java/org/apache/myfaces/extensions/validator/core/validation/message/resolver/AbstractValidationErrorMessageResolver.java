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

import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * MessageResolver which uses property files.
 * Subclasses just have to provide the package to look at.
 * An implementation can also provide a custom name which is e.g. configured via web.xml.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.CUSTOMIZABLE})
public abstract class AbstractValidationErrorMessageResolver implements MessageResolver
{
    public static final String MISSING_RESOURCE_MARKER = "???";

    protected final Logger logger = Logger.getLogger(getClass().getName());

    private static boolean deactivateDefaultConvention = ExtValCoreConfiguration.get().deactivateDefaultConvention();
    private static ResourceBundle defaultBundle = null;
    private String messageBundleBaseName;
    //with jsf 1.1 only available if there is a custom bean
    private String messageBundleVarName;

    protected AbstractValidationErrorMessageResolver()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    public String getMessage(String key, Locale locale)
    {
        if (key == null || key.equals(""))
        {
            return null;
        }

        if(key.contains(" "))
        {
            if(key.endsWith("_detail"))
            {
                key = key.substring(0, key.length() - 7);
            }
            return key;
        }

        String customMessage = null;

        try
        {
            customMessage = tryToFindCustomMessage(key, locale);
        }
        catch (Exception e)
        {
            //do nothing
        }

        if (customMessage != null)
        {
            return customMessage;
        }

        /*
         * try to use the convention for the message bundle
         */
        try
        {
            customMessage = tryToUseMessageBundleConvention(key, locale);
        }
        catch (Exception e)
        {
            //do nothing
        }

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
            customMessage = tryToFindCustomMessageInCustomResourceBundle(key, locale);
        }
        catch (Exception e)
        {
            //do nothing - it was just a try
        }

        return determineMessage(key, locale, customMessage);
    }

    private String tryToFindCustomMessage(String key, Locale locale)
    {
        ResourceBundle resourceBundle;
        String customMessage = null;

        //only in case of a ValidationErrorMessageResolver which is configured as bean
        if (this.messageBundleBaseName != null)
        {
            resourceBundle = ResourceBundle.getBundle(this.messageBundleBaseName, locale);
            if (resourceBundle != null)
            {
                customMessage = resourceBundle.getString(key);
            }
            else
            {
                logger.warning("message bundle " + this.messageBundleBaseName + " not found");
            }
        }

        //only in case of a ValidationErrorMessageResolver which is configured as bean
        if (this.messageBundleVarName != null && customMessage == null)
        {
            resourceBundle = (ResourceBundle) ExtValUtils.getELHelper().getBean(messageBundleVarName);

            if (resourceBundle != null)
            {
                customMessage = resourceBundle.getString(key);
            }
            else
            {
                logger.warning("message bundle var name " + this.messageBundleVarName + " not found");
            }
        }

        return customMessage;
    }

    private String tryToUseMessageBundleConvention(String key, Locale locale)
    {
        if (!deactivateDefaultConvention && isDefaultMessageBundleConventionActive())
        {
            if (defaultBundle == null)
            {
                try
                {
                    defaultBundle = ResourceBundle.getBundle(ExtValContext.getContext().getInformationProviderBean()
                        .get(CustomInformation.MESSAGE_BUNDLE_NAME), locale);
                }
                catch (Exception e)
                {
                    //do nothing
                    deactivateDefaultConvention = true;
                }
            }

            if (defaultBundle != null)
            {
                return defaultBundle.getString(key);
            }
        }

        return null;
    }

    private String tryToFindCustomMessageInCustomResourceBundle(String key, Locale locale)
    {
        ResourceBundle resourceBundle = tryToLoadCustomResourceBundle(locale);

        if (resourceBundle != null)
        {
            try
            {
                return resourceBundle.getString(key);
            }
            catch (MissingResourceException e)
            {
                logger.log(Level.FINEST, "no custom message for " + key + " within " + getCustomBaseName(), e);
            }
        }
        return null;
    }

    private ResourceBundle tryToLoadCustomResourceBundle(Locale locale)
    {
        String customBaseName = getCustomBaseName();

        if(customBaseName != null)
        {
            return ResourceBundle.getBundle(customBaseName, locale);
        }
        return null;
    }

    private String determineMessage(String key, Locale locale, String customMessage)
    {
        //use custom name (if possible) otherwise: fallback to default message (if possible)
        try
        {
            return (customMessage != null) ? customMessage
                    : (getBaseName() != null) ? ResourceBundle.getBundle(getBaseName(), locale).getString(key) : null;
        }
        catch (MissingResourceException e)
        {
            return MISSING_RESOURCE_MARKER + key + MISSING_RESOURCE_MARKER;
        }
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
