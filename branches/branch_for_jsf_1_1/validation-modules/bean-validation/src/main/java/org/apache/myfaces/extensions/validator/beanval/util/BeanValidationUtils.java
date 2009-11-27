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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.beanval.storage.ModelValidationEntry;
import org.apache.myfaces.extensions.validator.beanval.payload.ViolationSeverity;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.validation.message.FacesMessageHolder;
import org.apache.myfaces.extensions.validator.core.ProjectStage;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class BeanValidationUtils
{
    private static final Log LOGGER = LogFactory.getLog(BeanValidationUtils.class);
    private static ExtValBeanValidationMetaDataInternals bvmi = new ExtValBeanValidationMetaDataInternals(LOGGER);
    private static final String VALIDATOR_FACTORY_KEY = "javax.faces.validator.beanValidator.ValidatorFactory";

    public static void addMetaDataToContext(
            UIComponent component, PropertyDetails propertyDetails, boolean processModelValidation)
    {
        String[] key = propertyDetails.getKey().split("\\.");

        Object firstBean = ExtValUtils.getELHelper().getBean(key[0]);

        List<Class> foundGroupsForPropertyValidation = new ArrayList<Class>();
        List<Class> restrictedGroupsForPropertyValidation = new ArrayList<Class>();
        List<ModelValidationEntry> modelValidationEntryList = new ArrayList<ModelValidationEntry>();
        List<Class> restrictedGroupsForModelValidation = new ArrayList<Class>();

        bvmi.extractExtValBeanValidationMetaData(propertyDetails,
                processModelValidation,
                key,
                firstBean,
                foundGroupsForPropertyValidation,
                restrictedGroupsForPropertyValidation,
                modelValidationEntryList,
                restrictedGroupsForModelValidation);

        bvmi.processExtValBeanValidationMetaData(component,
                propertyDetails,
                foundGroupsForPropertyValidation,
                restrictedGroupsForPropertyValidation,
                modelValidationEntryList,
                restrictedGroupsForModelValidation);
    }

    public static void processConstraintViolations(FacesContext facesContext,
                                                   UIComponent uiComponent,
                                                   Object convertedObject,
                                                   Set<ConstraintViolation> violations)
    {
        List<FacesMessageHolder> facesMessageHolderList = new ArrayList<FacesMessageHolder>();

        FacesMessage facesMessage;
        for (ConstraintViolation violation : violations)
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
                                                                        ConstraintViolation violation)
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

    public static Class getWarnClass()
    {
        Object globalProperty = ExtValContext.getContext().getGlobalProperty(ViolationSeverity.Warn.class.getName());

        if(globalProperty instanceof Class)
        {
            return (Class)globalProperty;
        }

        tryToCreateMessageInDevMode(ViolationSeverity.Warn.class);

        return ViolationSeverity.Warn.class;
    }

    public static Class getInfoClass()
    {
        Object globalProperty = ExtValContext.getContext().getGlobalProperty(ViolationSeverity.Info.class.getName());

        if(globalProperty instanceof Class)
        {
            return (Class)globalProperty;
        }

        tryToCreateMessageInDevMode(ViolationSeverity.Info.class);

        return ViolationSeverity.Info.class;
    }

    public static Class getFatalClass()
    {
        Object globalProperty = ExtValContext.getContext().getGlobalProperty(ViolationSeverity.Fatal.class.getName());

        if(globalProperty instanceof Class)
        {
            return (Class)globalProperty;
        }

        tryToCreateMessageInDevMode(ViolationSeverity.Fatal.class);

        return ViolationSeverity.Fatal.class;
    }

    private static void tryToCreateMessageInDevMode(Class usedFallback)
    {
        String message = "[dev-mode warning] fallback to " + usedFallback.getName();

        if(ProjectStage.is(ProjectStage.Development))
        {
            FacesContext.getCurrentInstance()
                    .addMessage(null, ExtValUtils.createFacesMessage(FacesMessage.SEVERITY_WARN, message, message));
        }

        if(LOGGER.isWarnEnabled())
        {
            LOGGER.warn(message);
        }
    }

}
