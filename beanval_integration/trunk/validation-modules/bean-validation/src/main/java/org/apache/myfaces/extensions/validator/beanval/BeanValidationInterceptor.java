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

import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
import org.apache.myfaces.extensions.validator.core.recorder.ProcessedInformationRecorder;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.interceptor.AbstractRendererInterceptor;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.beanval.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.beanval.property.BeanValidationPropertyInformationKeys;
import org.apache.myfaces.extensions.validator.beanval.annotation.group.BeanValidationController;
import org.apache.myfaces.extensions.validator.beanval.annotation.group.ValidateGroup;
import org.apache.myfaces.extensions.validator.beanval.annotation.extractor.DefaultGroupControllerScanningExtractor;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.render.Renderer;
import javax.faces.convert.ConverterException;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import javax.validation.Validation;
import javax.validation.ValidatorFactory;
import javax.validation.ConstraintViolation;
import javax.validation.GroupSequence;
import java.util.Set;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.IOException;

/**
 * @author Gerhard Petracek
 * @since 1.x.3
 */
public class BeanValidationInterceptor extends AbstractRendererInterceptor
{
    private ValidatorFactory validationFactory = Validation.buildDefaultValidatorFactory();

    @Override
    @ToDo(Priority.HIGH)
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
    }

    @Override
    public void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
            throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        Object convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);

        //recorde user input e.g. for cross-component validation
        for(ProcessedInformationRecorder recorder : ExtValContext.getContext().getProcessedInformationRecorders())
        {
            recorder.recordUserInput(uiComponent, convertedObject);

            if(logger.isTraceEnabled())
            {
                logger.trace(recorder.getClass().getName() + " called");
            }
        }

        try
        {
            processValidation(facesContext, uiComponent, convertedObject);
        }
        catch (ValidatorException e)
        {
            throw new ConverterException(e.getFacesMessage(), e);
        }
    }

    protected void processValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject)
    {
        if (!(uiComponent instanceof EditableValueHolder))
        {
            return;
        }

        if(logger.isTraceEnabled())
        {
            logger.trace("jsr303 start validation");
        }

        if(!"true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_EMPTY_STRING_INTERPRETATION)
                && "".equals(convertedObject))
        {
            convertedObject = null;
        }

        MetaDataExtractor metaDataExtractor = ExtValUtils.getComponentMetaDataExtractor();

        PropertyInformation propertyInformation = metaDataExtractor.extract(facesContext, uiComponent);

        addGroupsToContext(propertyInformation, uiComponent.getClientId(facesContext));
        Class baseBeanClass = (propertyInformation.getInformation(
                PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class)).getBaseObject().getClass();

        String propertyName = (propertyInformation.getInformation(
                PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class)).getProperty();

        ExtValBeanValidationContext beanValidationContext = ExtValBeanValidationContext.getCurrentInstance();
        for(PropertyValidationInterceptor interceptor : beanValidationContext.getPropertyValidationInterceptors())
        {
            interceptor.beforeValidation(uiComponent, propertyInformation, convertedObject);
        }

        Set<ConstraintViolation> violations = this.validationFactory.usingContext()
                .messageInterpolator(ExtValBeanValidationContext.getCurrentInstance().getMessageInterpolator())
                .getValidator()
                .validateValue(
                        baseBeanClass,
                        propertyName,
                        convertedObject,
                        beanValidationContext.getGroups(
                                facesContext.getViewRoot().getViewId(),
                                uiComponent.getClientId(facesContext)));

        try
        {
            if(violations != null && violations.size() > 0)
            {
                ConstraintViolation violation = (ConstraintViolation) violations.toArray()[0];

                String violationMessage = violation.getMessage();

                String labeledMessage = "{0}: " + violationMessage;
                ValidatorException validatorException = new ValidatorException(
                        new FacesMessage(FacesMessage.SEVERITY_ERROR, labeledMessage, labeledMessage));

                ExtValUtils.executeAfterThrowingInterceptors(
                        uiComponent,
                        null,
                        convertedObject,
                        validatorException,
                        null);

                propertyInformation.setInformation(
                        BeanValidationPropertyInformationKeys.CONSTRAINT_VIOLATIONS, violations);

                if(labeledMessage.equals(validatorException.getFacesMessage().getSummary()) ||
                        labeledMessage.equals(validatorException.getFacesMessage().getDetail()))
                {
                    throw new ValidatorException(
                            new FacesMessage(FacesMessage.SEVERITY_ERROR, violationMessage, violationMessage));
                }
                else
                {
                    throw validatorException;
                }
            }
        }
        finally
        {
            for(PropertyValidationInterceptor interceptor : beanValidationContext.getPropertyValidationInterceptors())
            {
                interceptor.afterValidation(uiComponent, propertyInformation, convertedObject);
            }
        }

        if(logger.isTraceEnabled())
        {
            logger.trace("jsr303 validation finished");
        }
    }

    private void addGroupsToContext(PropertyInformation propertyInformation, String clientId)
    {
        PropertyDetails propertyDetails = propertyInformation
                .getInformation(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        String[] key = propertyDetails.getKey().split("\\.");

        Object firstBean = ExtValUtils.getELHelper().getBean(key[0]);

        List<GroupSequence> foundGroupSequencesForCurrentView = new ArrayList<GroupSequence>();

        if(!"true".equalsIgnoreCase(org.apache.myfaces.extensions.validator.beanval.WebXmlParameter
                .DEACTIVATE_ADDITIONAL_GROUP_VALIDATION_ANNOTATIONS))
        {
            processClass(firstBean.getClass(), foundGroupSequencesForCurrentView, false);
            processClass(propertyDetails.getBaseObject().getClass(), foundGroupSequencesForCurrentView, false);
            processFieldsAndProperties(key[0] + "." + key[1], firstBean, key[1], foundGroupSequencesForCurrentView);
            processFieldsAndProperties(
                    propertyDetails.getKey(),
                    propertyDetails.getBaseObject(),
                    propertyDetails.getProperty(),
                    foundGroupSequencesForCurrentView);
        }
        else
        {
            processClass(firstBean.getClass(), foundGroupSequencesForCurrentView, true);
            processClass(propertyDetails.getBaseObject().getClass(), foundGroupSequencesForCurrentView, true);
        }

        for(GroupSequence currentGroupSequence : foundGroupSequencesForCurrentView)
        {
            for(Class currentGroup : currentGroupSequence.value())
            {
                ExtValBeanValidationContext.getCurrentInstance()
                        .addGroup(currentGroup, FacesContext.getCurrentInstance().getViewRoot().getViewId(), clientId);
            }
        }
    }

    //TODO process super classes and interfaces as well
    private void processClass(
            Class classToInspect, List<GroupSequence> foundGroupSequencesForCurrentView, boolean useStandardAnnotations)
    {
        if(useStandardAnnotations)
        {
            if(classToInspect.isAnnotationPresent(GroupSequence.class))
            {
                foundGroupSequencesForCurrentView
                        .add((GroupSequence) classToInspect.getAnnotation(GroupSequence.class));
            }
        }
        else
        {
            if(classToInspect.isAnnotationPresent(BeanValidationController.class))
            {
                addGroupSequencesForCurrentView(
                        (BeanValidationController) classToInspect.getAnnotation(BeanValidationController.class),
                        foundGroupSequencesForCurrentView);
            }
        }
    }

    private void processFieldsAndProperties(
            String key, Object base, String property, List<GroupSequence> foundGroupSequencesForCurrentView)
    {
        PropertyInformation propertyInformation = new DefaultGroupControllerScanningExtractor()
                .extract(FacesContext.getCurrentInstance(), new PropertyDetails(key, base, property));

        for(MetaDataEntry metaDataEntry : propertyInformation.getMetaDataEntries())
        {
            if(metaDataEntry.getValue() instanceof BeanValidationController)
            {
                addGroupSequencesForCurrentView(
                        (BeanValidationController)metaDataEntry.getValue(), foundGroupSequencesForCurrentView);
            }
        }
    }

    private void addGroupSequencesForCurrentView(
            BeanValidationController beanValidationController, List<GroupSequence> foundGroupSequencesForCurrentView)
    {
        for(ValidateGroup validateGroup : beanValidationController.value())
        {
            for(String currentViewIdEntry : validateGroup.viewId())
            {
                if(currentViewIdEntry.equals(FacesContext.getCurrentInstance().getViewRoot().getViewId()) ||
                        currentViewIdEntry.equals("*"))
                {
                    foundGroupSequencesForCurrentView.addAll(Arrays.asList(validateGroup.groupSequence()));
                }
            }
        }
    }
}
