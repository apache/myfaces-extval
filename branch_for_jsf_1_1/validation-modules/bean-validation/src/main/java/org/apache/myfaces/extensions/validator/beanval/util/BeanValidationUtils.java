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
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class BeanValidationUtils
{
    private static final Log LOG = LogFactory.getLog(BeanValidationUtils.class);
    private static ExtValBeanValidationMetaDataInternals bvmi = new ExtValBeanValidationMetaDataInternals(LOG);

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
}
