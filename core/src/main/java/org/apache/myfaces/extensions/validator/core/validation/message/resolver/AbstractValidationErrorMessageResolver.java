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

import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.logging.Log;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

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

    protected final Log logger = LogFactory.getLog(getClass());

    private static String deactivateDefaultConvention = WebXmlParameter.DEACTIVATE_DEFAULT_CONVENTION;
    private static ResourceBundle defaultBundle = null;
    private String messageBundleBaseName;
    //with jsf 1.1 only available if there is a custom bean
    private String messageBundleVarName;

    protected AbstractValidationErrorMessageResolver()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
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

        ResourceBundle resourceBundle = null;
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
                if(logger.isWarnEnabled())
                {
                    logger.warn("message bundle " + this.messageBundleBaseName + " not found");
                }
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
                if(logger.isWarnEnabled())
                {
                    logger.warn("message bundle var name " + this.messageBundleVarName + " not found");
                }
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
            resourceBundle = ResourceBundle.getBundle(getCustomBaseName(), locale);
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
                if(logger.isTraceEnabled())
                {
                    logger.trace("no custom message for " + key + " within " + getCustomBaseName(), e);
                }
            }
        }

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

    private String tryToUseMessageBundleConvention(String key, Locale locale)
    {
        String customMessage = null;

        if ((deactivateDefaultConvention == null || !deactivateDefaultConvention.equalsIgnoreCase("true"))
            && isDefaultMessageBundleConventionActive())
        {
            if (defaultBundle == null)
            {
                try
                {
                    defaultBundle = ResourceBundle.getBundle(ExtValContext.getContext().getInformationProviderBean()
                        .get(CustomInformation.MESSAGE_BUNDLE_NAME), locale);
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
