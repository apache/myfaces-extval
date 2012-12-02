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
package org.apache.myfaces.extensions.validator.beanval.util;

import org.apache.myfaces.extensions.validator.beanval.storage.ModelValidationEntry;
import org.apache.myfaces.extensions.validator.beanval.ExtValBeanValidationContext;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.validation.message.FacesMessageHolder;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.validation.ConstraintViolation;
import javax.validation.ValidatorFactory;
import javax.validation.Validation;
import javax.validation.metadata.ElementDescriptor;
import javax.validation.metadata.PropertyDescriptor;
import javax.validation.metadata.BeanDescriptor;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.Collections;
import java.util.logging.Logger;

/**
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class BeanValidationUtils
{
    private static final Logger LOGGER = Logger.getLogger(BeanValidationUtils.class.getName());
    private static ELHelper elHelper = ExtValUtils.getELHelper();

    private static ExtValBeanValidationMetaDataInternals bvmi =
            new ExtValBeanValidationMetaDataInternals(LOGGER, elHelper);

    private static final String VALIDATOR_FACTORY_KEY = "javax.faces.validator.beanValidator.ValidatorFactory";

    @SuppressWarnings({"unchecked"})
    public static Set<ConstraintViolation<Object>> validate(Class baseClass,
                                                            String propertyName,
                                                            Object objectToValidate,
                                                            Class[] groups,
                                                            boolean cascadedValidation)
    {
        ValidatorFactory validatorFactory = ExtValBeanValidationContext.getCurrentInstance().getValidatorFactory();
        Set<ConstraintViolation<Object>> result =
                validatorFactory.usingContext()
                .messageInterpolator(ExtValBeanValidationContext.getCurrentInstance().getMessageInterpolator())
                .constraintValidatorFactory(validatorFactory.getConstraintValidatorFactory())
                .traversableResolver(validatorFactory.getTraversableResolver())
                .getValidator()
                .validateValue(baseClass, propertyName, objectToValidate, groups);

        if(result.isEmpty() && cascadedValidation && objectToValidate != null)
        {
            result = processCascadedValidation(validatorFactory, baseClass, propertyName, objectToValidate, groups);
        }

        return result;
    }

    private static Set<ConstraintViolation<Object>> processCascadedValidation(ValidatorFactory validatorFactory,
                                                                              Class baseBeanClass,
                                                                              String propertyName,
                                                                              Object objectToValidate,
                                                                              Class[] groups)
    {
        ElementDescriptor elementDescriptor = getElementDescriptor(baseBeanClass, propertyName);

        if(elementDescriptor instanceof PropertyDescriptor && ((PropertyDescriptor)elementDescriptor).isCascaded())
        {
            return validatorFactory.usingContext()
                .messageInterpolator(ExtValBeanValidationContext.getCurrentInstance().getMessageInterpolator())
                .constraintValidatorFactory(validatorFactory.getConstraintValidatorFactory())
                .traversableResolver(validatorFactory.getTraversableResolver())
                .getValidator()
                .validate(objectToValidate, groups);
        }
        return Collections.emptySet();
    }

    public static ElementDescriptor getElementDescriptor(Class targetClass, String property)
    {
        BeanDescriptor beanDescriptor = ExtValBeanValidationContext.getCurrentInstance().getValidatorFactory()
                .getValidator().getConstraintsForClass(targetClass);

        return beanDescriptor.getConstraintsForProperty(property);
    }

    public static void addMetaDataToContext(
            UIComponent component, PropertyDetails propertyDetails, boolean processModelValidation)
    {
        String[] key = propertyDetails.getKey().split("\\.");

        Object firstBean = elHelper.getBean(key[0]);

        List<Class> foundGroupsForPropertyValidation = new ArrayList<Class>();
        List<Class> restrictedGroupsForPropertyValidation = new ArrayList<Class>();
        List<ModelValidationEntry> modelValidationEntryList = new ArrayList<ModelValidationEntry>();
        List<Class> restrictedGroupsForModelValidation = new ArrayList<Class>();

        String activeViewId = FacesContext.getCurrentInstance().getViewRoot().getViewId();

        bvmi.extractExtValBeanValidationMetaData(propertyDetails,
                processModelValidation,
                key,
                firstBean,
                foundGroupsForPropertyValidation,
                restrictedGroupsForPropertyValidation,
                modelValidationEntryList,
                restrictedGroupsForModelValidation,
                activeViewId);

        bvmi.processExtValBeanValidationMetaData(component,
                propertyDetails,
                foundGroupsForPropertyValidation,
                restrictedGroupsForPropertyValidation,
                modelValidationEntryList,
                restrictedGroupsForModelValidation,
                activeViewId);
    }

    public static void processConstraintViolations(FacesContext facesContext,
                                                   UIComponent uiComponent,
                                                   Object convertedObject,
                                                   Set<ConstraintViolation<Object>> violations)
    {
        List<FacesMessageHolder> facesMessageHolderList = new ArrayList<FacesMessageHolder>();

        FacesMessage facesMessage;
        for (ConstraintViolation<Object> violation : violations)
        {
            facesMessage = createFacesMessageForConstraintViolation(uiComponent, convertedObject, violation);

            if (facesMessage == null)
            {
                continue;
            }

            bvmi.processFacesMessage(facesContext, uiComponent, facesMessageHolderList, facesMessage);
        }

        processViolationMessages(facesMessageHolderList);
    }

    public static FacesMessage createFacesMessageForConstraintViolation(UIComponent uiComponent,
                                                                        Object convertedObject,
                                                                        ConstraintViolation<Object> violation)
    {
        String violationMessage = violation.getMessage();

        String labeledMessageSummary = bvmi.createLabeledMessage(violationMessage, false);
        String labeledMessageDetail = bvmi.createLabeledMessage(violationMessage, true);

        FacesMessage.Severity severity = bvmi.calcSeverity(violation);

        ValidatorException validatorException = bvmi
                .createValidatorException(labeledMessageSummary, labeledMessageDetail, severity);

        if (!bvmi.executeAfterThrowingInterceptors(uiComponent, convertedObject, validatorException))
        {
            return null;
        }

        if (bvmi.isMessageTextUnchanged(validatorException, labeledMessageSummary, labeledMessageDetail))
        {
            return ExtValUtils.createFacesMessage(severity, violationMessage, violationMessage);
        }
        else
        {
            return ExtValUtils.createFacesMessage(severity,
                    validatorException.getFacesMessage().getSummary(),
                    validatorException.getFacesMessage().getDetail());
        }
    }

    public static void processViolationMessages(List<FacesMessageHolder> violationMessageHolderList)
    {
        if (violationMessageHolderList == null || violationMessageHolderList.isEmpty())
        {
            return;
        }

        List<FacesMessageHolder> facesMessageListWithLowSeverity =
                bvmi.getFacesMessageListWithLowSeverity(violationMessageHolderList);
        List<FacesMessageHolder> facesMessageListWithHighSeverity =
                bvmi.getFacesMessageListWithHighSeverity(violationMessageHolderList);

        bvmi.addMessages(facesMessageListWithHighSeverity);
        bvmi.addMessages(facesMessageListWithLowSeverity);
    }

    public static ValidatorFactory getDefaultValidatorFactory()
    {
        Map<String, Object> applicationMap = FacesContext.getCurrentInstance().getExternalContext().getApplicationMap();
        ValidatorFactory validatorFactory = null;

        if (applicationMap.containsKey(VALIDATOR_FACTORY_KEY))
        {
            if (applicationMap.get(VALIDATOR_FACTORY_KEY) instanceof ValidatorFactory)
            {
                validatorFactory = (ValidatorFactory) applicationMap.get(VALIDATOR_FACTORY_KEY);
            }
        }

        if (validatorFactory == null)
        {
            validatorFactory = Validation.buildDefaultValidatorFactory();
            applicationMap.put(VALIDATOR_FACTORY_KEY, validatorFactory);
        }
        return validatorFactory;
    }
}
