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
package org.apache.myfaces.extensions.validator.beanval;

import org.apache.commons.logging.Log;
import org.apache.myfaces.extensions.validator.beanval.validation.strategy.BeanValidationVirtualValidationStrategy;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.validation.ConstraintViolation;
import javax.validation.groups.Default;
import javax.validation.metadata.BeanDescriptor;
import javax.validation.metadata.ConstraintDescriptor;
import javax.validation.metadata.ElementDescriptor;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
class BeanValidationInterceptorInternals
{
    private Log logger;

    BeanValidationInterceptorInternals(Log logger)
    {
        this.logger = logger;
    }

    PropertyDetails extractPropertyDetails(FacesContext facesContext, UIComponent uiComponent)
    {
        PropertyDetails result = ExtValUtils.getComponentMetaDataExtractor().extract(facesContext, uiComponent)
                .getInformation(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        if (result.getBaseObject() == null && this.logger.isWarnEnabled())
        {
            this.logger.warn("no base object at " + result.getKey() +
                    " component-id: " + uiComponent.getClientId(facesContext));
        }

        return result.getBaseObject() != null ? result : null;
    }

    void initComponentWithPropertyDetails(
            FacesContext facesContext, UIComponent uiComponent, PropertyDetails propertyDetails)
    {
        Class[] foundGroups = resolveGroups(facesContext, uiComponent);

        if (foundGroups == null)
        {
            return;
        }
        else if(foundGroups.length == 0)
        {
            foundGroups = new Class[]{Default.class};
        }

        ElementDescriptor elementDescriptor = getDescriptorFor(
                propertyDetails.getBaseObject().getClass(), propertyDetails.getProperty());

        if (elementDescriptor == null)
        {
            return;
        }

        Map<String, Object> metaData;

        for (ConstraintDescriptor<?> constraintDescriptor :
                elementDescriptor.findConstraints().unorderedAndMatchingGroups(foundGroups).getConstraintDescriptors())
        {
            metaData = transformConstraintDescriptorToMetaData(
                    constraintDescriptor, elementDescriptor.getElementClass());

            if (metaData != null && !metaData.isEmpty())
            {
                ExtValUtils.configureComponentWithMetaData(facesContext, uiComponent, metaData);
            }
        }
    }

    private Map<String, Object> transformConstraintDescriptorToMetaData(
            ConstraintDescriptor<?> constraintDescriptor, Class elementClass)
    {
        Map<String, Object> result = null;
        MetaDataTransformer metaDataTransformer;
        MetaDataEntry entry;

        metaDataTransformer = ExtValUtils.getMetaDataTransformerForValidationStrategy(
                new BeanValidationVirtualValidationStrategy(constraintDescriptor, elementClass));

        if (metaDataTransformer != null)
        {
            if (this.logger.isDebugEnabled())
            {
                this.logger.debug(metaDataTransformer.getClass().getName() + " instantiated");
            }

            entry = new MetaDataEntry();
            entry.setKey(constraintDescriptor.getAnnotation().annotationType().getName());
            entry.setValue(constraintDescriptor);

            result = metaDataTransformer.convertMetaData(entry);
        }
        return result;
    }

    boolean hasBeanValidationConstraints(PropertyInformation propertyInformation)
    {
        PropertyDetails propertyDetails = ExtValUtils.getPropertyDetails(propertyInformation);

        return getDescriptorFor(propertyDetails.getBaseObject().getClass(), propertyDetails.getProperty()) != null;
    }

    @SuppressWarnings({"unchecked"})
    void validate(FacesContext facesContext,
                  UIComponent uiComponent,
                  Object convertedObject,
                  PropertyInformation propertyInformation, boolean supportMultipleViolationsPerField)
    {
        Class baseBeanClass = getBaseClassType(propertyInformation);
        String propertyName = getPropertyToValidate(propertyInformation);

        Class[] groups = resolveGroups(facesContext, uiComponent);

        if (groups == null)
        {
            return;
        }

        Set<ConstraintViolation> violations = ExtValBeanValidationContext.getCurrentInstance().getValidatorFactory()
                .usingContext()
                .messageInterpolator(ExtValBeanValidationContext.getCurrentInstance().getMessageInterpolator())
                .getValidator()
                .validateValue(baseBeanClass, propertyName, convertedObject, groups);

        processConstraintViolations(
                facesContext, uiComponent, convertedObject, violations, supportMultipleViolationsPerField);
    }

    private void processConstraintViolations(FacesContext facesContext,
                                             UIComponent uiComponent,
                                             Object convertedObject,
                                             Set<ConstraintViolation> violations,
                                             boolean supportMultipleViolationsPerField)
    {
        List<String> violationMessages = new ArrayList<String>();
        for (ConstraintViolation violation : violations)
        {
            processConstraintViolation(uiComponent, convertedObject, violation, violationMessages);

            if (!supportMultipleViolationsPerField)
            {
                break;
            }
        }

        if (!violationMessages.isEmpty())
        {
            throwException(facesContext, uiComponent, violationMessages, supportMultipleViolationsPerField);
        }
    }

    private void processConstraintViolation(UIComponent uiComponent,
                                            Object convertedObject,
                                            ConstraintViolation violation,
                                            List<String> violationMessages)
    {
        String violationMessage = violation.getMessage();

        String labeledMessage = "{0}: " + violationMessage;
        ValidatorException validatorException = createValidatorException(labeledMessage);

        executeAfterThrowingInterceptors(uiComponent, convertedObject, validatorException);

        //check if the message has changed
        if (isMessageTextUnchanged(validatorException, labeledMessage))
        {
            violationMessages.add(violationMessage);
        }
        else
        {
            violationMessages.add(validatorException.getFacesMessage().getSummary());
        }
    }

    private void executeAfterThrowingInterceptors(UIComponent uiComponent,
                                                  Object convertedObject,
                                                  ValidatorException validatorException)
    {
        ExtValUtils.executeAfterThrowingInterceptors(
                uiComponent,
                null,
                convertedObject,
                validatorException,
                null);
    }

    private boolean isMessageTextUnchanged(ValidatorException validatorException, String violationMessage)
    {
        return violationMessage.equals(validatorException.getFacesMessage().getSummary()) ||
                violationMessage.equals(validatorException.getFacesMessage().getDetail());
    }

    private ValidatorException createValidatorException(String violationMessage)
    {
        return new ValidatorException(
                ExtValUtils.createFacesMessage(FacesMessage.SEVERITY_ERROR, violationMessage, violationMessage));
    }

    private Class getBaseClassType(PropertyInformation propertyInformation)
    {
        return ExtValUtils.getPropertyDetails(propertyInformation).getBaseObject().getClass();
    }

    private String getPropertyToValidate(PropertyInformation propertyInformation)
    {
        return ExtValUtils.getPropertyDetails(propertyInformation).getProperty();
    }

    //override this method in the jsf 2.0 version
    private void throwException(FacesContext facesContext, UIComponent uiComponent,
                                List<String> violationMessages, boolean supportMultipleViolationsPerField)
    {
        if (supportMultipleViolationsPerField)
        {
            boolean firstMessage = false;
            for (String message : violationMessages)
            {
                if (!firstMessage)
                {
                    firstMessage = true;
                }
                else
                {
                    facesContext.addMessage(uiComponent.getClientId(facesContext),
                            ExtValUtils.createFacesMessage(FacesMessage.SEVERITY_ERROR, message, message));
                }
            }
        }

        throw new ValidatorException(ExtValUtils.createFacesMessage(
                FacesMessage.SEVERITY_ERROR, violationMessages.get(0), violationMessages.get(0)));
    }

    private Class[] resolveGroups(FacesContext facesContext, UIComponent uiComponent)
    {
        return ExtValBeanValidationContext.getCurrentInstance().getGroups(
                facesContext.getViewRoot().getViewId(),
                uiComponent.getClientId(facesContext));
    }

    private ElementDescriptor getDescriptorFor(Class targetClass, String property)
    {
        BeanDescriptor beanDescriptor = ExtValBeanValidationContext.getCurrentInstance().getValidatorFactory()
                .getValidator().getConstraintsForClass(targetClass);

        return beanDescriptor.getConstraintsForProperty(property);
    }
}
