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
package org.apache.myfaces.extensions.validator.core.validation.strategy;

import org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.mapper.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.LogUtils;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import java.lang.annotation.Annotation;
import java.util.Locale;
import java.util.MissingResourceException;

/**
 * Provides the ability of message resolving to ValidationStrategies
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.REUSE})
public abstract class AbstractValidationStrategy extends
    AbstractValidatorAdapter
{
    protected static final String DETAIL_MESSAGE_KEY_POSTFIX = "_details";
    private MessageResolver messageResolver;

    protected String resolveMessage(String key)
    {
        Locale locale = FacesContext.getCurrentInstance().getViewRoot().getLocale();

        return this.messageResolver != null ? this.messageResolver.getMessage(key, locale) :
            ((ClassMappingFactory<ValidationStrategy, MessageResolver>)ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, ClassMappingFactory.class))
                .create(this)
                .getMessage(key, locale);
    }

    protected String getErrorMessageSummary(Annotation annotation)
    {
        return resolveMessage(getValidationErrorMsgKey(annotation));
    }

    protected String getErrorMessageDetails(Annotation annotation)
    {
        try
        {
            String key = getValidationErrorMsgKey(annotation);
            return (key != null) ? resolveMessage(key + DETAIL_MESSAGE_KEY_POSTFIX) : null;
        }
        catch (MissingResourceException e)
        {
            LogUtils.warn("couldn't find key " + getValidationErrorMsgKey(annotation) + DETAIL_MESSAGE_KEY_POSTFIX, e,
                getClass());
        }
        return null;
    }

    protected FacesMessage getValidationErrorFacesMassage(Annotation annotation)
    {
        return new FacesMessage(FacesMessage.SEVERITY_ERROR,
            getErrorMessageSummary(annotation), getErrorMessageDetails(annotation));
    }

    protected abstract String getValidationErrorMsgKey(Annotation annotation);

    public void setMessageResolver(MessageResolver messageResolver)
    {
        this.messageResolver = messageResolver;
    }
}
