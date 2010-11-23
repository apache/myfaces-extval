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
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;
import java.lang.annotation.Annotation;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.logging.Level;

/**
 * This class provides an easier handling of violation messages.
 * This abstract class is a good candidate as super-class for custom validation strategies.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.REUSE})
public abstract class AbstractAnnotationValidationStrategy<A extends Annotation> extends AbstractValidationStrategy
{
    protected static final String DETAIL_MESSAGE_KEY_POSTFIX = "_detail";
    //e.g. for injecting a message resolver via spring
    private MessageResolver messageResolver;

    /**
     * Uses the given message key to resolve the actual message in the correct language.
     * The current {@link javax.faces.component.UIViewRoot} provides the target locale.
     * When a {@link org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver}
     * is available, it is used to resolve the message.
     * Otherwise the default rules are taken to define the
     * {@link org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver}.
     * {@link org.apache.myfaces.extensions.validator.util.ExtValUtils#getMessageResolverForValidationStrategy(
     * org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy)}
     *
     * @param key key of the error message that needs to be resolved.
     * @return Resolved message.
     */
    protected String resolveMessage(String key)
    {
        Locale locale = FacesContext.getCurrentInstance().getViewRoot().getLocale();

        return this.messageResolver != null ? this.messageResolver.getMessage(key, locale) :
            ExtValUtils.getMessageResolverForValidationStrategy(this).getMessage(key, locale);
    }

    /**
     * Returns the error message (the summary) provided by the given constraint. The message is determined
     * by using the {@link #getValidationErrorMsgKey} method for obtaining the message-/key which then is
     * resolved by the method {@link #resolveMessage(String) resolveMessage}.
     *
     * @param annotation The annotation associated with the
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}.
     * @return The error message used in the summary part of the
     * {@link javax.faces.application.FacesMessage FacesMessage}.
     */
    protected String getErrorMessageSummary(A annotation)
    {
        return resolveMessage(getValidationErrorMsgKey(annotation));
    }

    /**
     * Returns the error message (the detailed message) provided by the given constraint. The message is determined
     * by using the {@link #getValidationErrorMsgKey} method for obtaining the message-/key which then is
     * resolved by the method {@link #resolveMessage(String) resolveMessage}.
     * When the key isn't found, the resulting exception is just logged but not propagated.
     * So the detail message isn't required and null is returned instead.
     *
     * @param annotation The annotation associated with the
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}.
     * @return The error message used in the detail part of the
     * {@link javax.faces.application.FacesMessage FacesMessage} or null.
     */
    @ToDo(value = Priority.MEDIUM, description = "just log it in case of project stage development")
    protected String getErrorMessageDetail(A annotation)
    {
        try
        {
            String key = getValidationErrorMsgKey(annotation);
            return (key != null) ? resolveMessage(key + DETAIL_MESSAGE_KEY_POSTFIX) : null;
        }
        catch (MissingResourceException e)
        {
            logger.log(Level.WARNING,
                    "couldn't find key " + getValidationErrorMsgKey(annotation) + DETAIL_MESSAGE_KEY_POSTFIX, e);
        }
        return null;
    }

    /**
     * Creates the {@link javax.faces.application.FacesMessage FacesMessage} that
     * will be used to inform the user of a validation error. This method can be called by
     * subclasses in the {@link #processValidation(FacesContext, UIComponent, MetaDataEntry, Object) processValidation}
     * method when a {@link ValidatorException} is thrown.
     *
     * @param annotation The annotation associated with the
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}.
     * @return FacesMessage for informing user of the problem.
     */
    protected FacesMessage getValidationErrorFacesMessage(A annotation)
    {
        return ExtValUtils.createFacesMessage(getErrorMessageSummary(annotation), getErrorMessageDetail(annotation));
    }

    /**
     * Returns the key for the message that needs to be resolved for the violation-message.
     *
     * @param annotation The annotation associated with the
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}.
     * @return The key of the error message.
     */
    protected abstract String getValidationErrorMsgKey(A annotation);

    /**
     * Injection point for a
     * {@link org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver}
     * that needs to be used by this
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}.
     *
     * @param messageResolver messageResolver that needs to be used by this validationStrategy.
     */
    @SuppressWarnings({"UnusedDeclaration"})
    public void setMessageResolver(MessageResolver messageResolver)
    {
        this.messageResolver = messageResolver;
    }

    /**
     * {@inheritDoc}
     * Adds the label of the component to the
     * {@link org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry}.
     */
    @Override
    protected boolean processAfterValidatorException(FacesContext facesContext,
                                                     UIComponent uiComponent,
                                                     MetaDataEntry metaDataEntry,
                                                     Object convertedObject,
                                                     ValidatorException validatorException)
    {
        metaDataEntry.setProperty(PropertyInformationKeys.LABEL, getLabel(facesContext, uiComponent, metaDataEntry));

        return super.processAfterValidatorException(
                facesContext, uiComponent, metaDataEntry, convertedObject, validatorException);
    }

    /**
     * Returns the label of the uiComponent field that could be used in the error messages. By default it returns null.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The JSF component that contained the value entered by the user.
     * @param metaDataEntry The data holder which stores the meta-data and some information where the meta-data was
     * around.
     * @return The label-text of the {@link javax.faces.component.UIComponent} which can be used in the violation msg.
     */
    //e.g. for custom annotations - override if needed
    protected String getLabel(FacesContext facesContext, UIComponent uiComponent, MetaDataEntry metaDataEntry)
    {
        return null;
    }
}
