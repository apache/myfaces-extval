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
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.interceptor.AbstractValidationInterceptor;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.beanval.validation.strategy.BeanValidationStrategyAdapter;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.render.Renderer;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import javax.validation.Validation;
import javax.validation.metadata.BeanDescriptor;
import javax.validation.metadata.ElementDescriptor;
import javax.validation.metadata.ConstraintDescriptor;
import javax.validation.ValidatorFactory;
import javax.validation.ConstraintViolation;
import java.util.Set;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.io.IOException;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@ToDo(value = Priority.HIGH, description = "sync jsf 2.0 specific changes with bv-branch")
@UsageInformation(UsageCategory.INTERNAL)
public class BeanValidationInterceptor extends AbstractValidationInterceptor
{
    private ValidatorFactory validationFactory = Validation.buildDefaultValidatorFactory();

    @Override
    @ToDo.List({
     @ToDo(value = Priority.HIGH, description = "the api is available - but hv v4 beta doesn't impl. the needed parts"),
     @ToDo(value = Priority.HIGH, description = "remove this overridden method")})
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        if(processComponent(uiComponent))
        {
            //initComponent(facesContext, uiComponent);
        }
    }

    @ToDo(value = Priority.HIGH, description = "check groups")
    protected void initComponent(FacesContext facesContext, UIComponent uiComponent)
    {
        if (logger.isTraceEnabled())
        {
            logger.trace("start to init component " + uiComponent.getClass().getName());
        }

        MetaDataExtractor metaDataExtractor = ExtValUtils.getComponentMetaDataExtractor();

        PropertyDetails propertyDetails = metaDataExtractor.extract(facesContext, uiComponent)
                .getInformation(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        if(propertyDetails.getBaseObject() == null)
        {
            if(this.logger.isWarnEnabled())
            {
                this.logger.warn("no base object at " + propertyDetails.getKey() +
                        " component-id: " + uiComponent.getClientId(facesContext));
            }
            return;
        }

        //TODO extract groups - see PropertyValidationGroupProvider
        Class[] foundGroups = ExtValBeanValidationContext.getCurrentInstance().getGroups(
                facesContext.getViewRoot().getViewId(),
                uiComponent.getClientId(facesContext));

        /*TODO
            foundGroups = mergeFoundGroupsWithValidatorGroups(
                foundGroups, ((EditableValueHolder)uiComponent).getValidators());
        */
        BeanDescriptor beanDescriptor = this.validationFactory.getValidator().getConstraintsForClass(
                propertyDetails.getBaseObject().getClass());

        ElementDescriptor elementDescriptor = beanDescriptor.getConstraintsForProperty(propertyDetails.getProperty());

        if(elementDescriptor == null)
        {
            return;
        }
        
        ValidationStrategy validationStrategy;
        MetaDataTransformer metaDataTransformer;
        MetaDataEntry entry;
        Map<String, Object> metaData;

        for (ConstraintDescriptor<?> constraintDescriptor :
                elementDescriptor.getUnorderdConstraintDescriptorsMatchingGroups(foundGroups))
        {
            //TODO check groups
            
            validationStrategy = new BeanValidationStrategyAdapter(constraintDescriptor);

            /*
             * per default
             * org.apache.myfaces.extensions.validator.beanval.metadata.transformer.BeanValidationMetaDataTransformer
             * is bound to BeanValidationStrategyAdapter
             * don't use it directly - it's possible to deactivate
             * org.apache.myfaces.extensions.validator.beanval.metadata.transformer.mapper
             *    .DefaultBeanValidationStrategyToMetaDataTransformerNameMapper
             */
            metaDataTransformer = ExtValUtils.getMetaDataTransformerForValidationStrategy(validationStrategy);

            if (metaDataTransformer != null)
            {
                if (this.logger.isDebugEnabled())
                {
                    this.logger.debug(metaDataTransformer.getClass().getName() + " instantiated");
                }

                entry = new MetaDataEntry();
                entry.setKey(constraintDescriptor.getAnnotation().annotationType().getName());
                entry.setValue(constraintDescriptor);
                //TODO (?) add type of property for meta-data transformation (e.g. size: string vs. number)

                metaData = metaDataTransformer.convertMetaData(entry);
            }
            else
            {
                metaData = null;
            }

            if (metaData == null)
            {
                continue;
            }

            //get component initializer for the current component and configure it
            //also in case of skipped validation to reset e.g. the required attribute
            if (!metaData.isEmpty())
            {
                ExtValUtils.configureComponentWithMetaData(facesContext, uiComponent, metaData);
            }
        }

        if (logger.isTraceEnabled())
        {
            logger.trace("init component of " + uiComponent.getClass().getName() + " finished");
        }
    }

    @Override
    protected boolean recordProcessedInformation()
    {
        return false;
    }

    @ToDo(value = Priority.HIGH, description = "use ExtValUtils#createFacesMessage")
    protected void processValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject)
    {
        MetaDataExtractor metaDataExtractor = ExtValUtils.getComponentMetaDataExtractor();

        PropertyInformation propertyInformation = metaDataExtractor.extract(facesContext, uiComponent);

        boolean validateProperty = processBeanValidationForProperty(propertyInformation);
        try
        {
            if(validateProperty)
            {
                if (logger.isTraceEnabled())
                {
                    logger.trace("jsr303 start validation");
                }

                //e.g.: extract groups for validation
                if(!ExtValUtils.executeGlobalBeforeValidationInterceptors(facesContext, uiComponent, convertedObject,
                        PropertyInformation.class.getName(), propertyInformation, BeanValidationModuleKey.class))
                {
                    return;
                }

                processFieldValidation(facesContext, uiComponent, convertedObject, propertyInformation);
            }
        }
        finally
        {
            if(validateProperty)
            {
                if (logger.isTraceEnabled())
                {
                    logger.trace("jsr303 validation finished");
                }

                ExtValUtils.executeGlobalAfterValidationInterceptors(facesContext, uiComponent, convertedObject,
                        PropertyInformation.class.getName(), propertyInformation, BeanValidationModuleKey.class);
            }
        }
    }

    protected boolean processBeanValidationForProperty(PropertyInformation propertyInformation)
    {
        PropertyDetails propertyDetails = (propertyInformation.getInformation(
                PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class));

        BeanDescriptor beanDescriptor = this.validationFactory.getValidator().getConstraintsForClass(
                propertyDetails.getBaseObject().getClass());

        ElementDescriptor elementDescriptor = beanDescriptor.getConstraintsForProperty(propertyDetails.getProperty());

        return elementDescriptor != null;
    }

    protected void processFieldValidation(FacesContext facesContext,
                                          UIComponent uiComponent,
                                          Object convertedObject,
                                          PropertyInformation propertyInformation)
    {
        Class baseBeanClass = (propertyInformation.getInformation(
                PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class)).getBaseObject().getClass();

        String propertyName = (propertyInformation.getInformation(
                PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class)).getProperty();

        ExtValBeanValidationContext beanValidationContext = ExtValBeanValidationContext.getCurrentInstance();

        Class[] groups = beanValidationContext.getGroups(
                facesContext.getViewRoot().getViewId(), uiComponent.getClientId(facesContext));

        if(groups == null)
        {
            return;
        }

        Set<ConstraintViolation> violations = this.validationFactory.usingContext()
                .messageInterpolator(ExtValBeanValidationContext.getCurrentInstance().getMessageInterpolator())
                .getValidator()
                .validateValue(baseBeanClass, propertyName, convertedObject, groups);

        List<String> violationMessages = new ArrayList<String>();
        for(ConstraintViolation violation : violations)
        {
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

            //check if the message has changed
            if (labeledMessage.equals(validatorException.getFacesMessage().getSummary()) ||
                    labeledMessage.equals(validatorException.getFacesMessage().getDetail()))
            {
                violationMessages.add(violationMessage);
            }
            else
            {
                violationMessages.add(validatorException.getFacesMessage().getSummary());
            }

            if(!supportMultipleViolationsPerField())
            {
                break;
            }
        }

        if(!violationMessages.isEmpty())
        {
            throwException(facesContext, uiComponent, violationMessages);
        }
    }

    //override this method in the jsf 2.0 version
    protected void throwException(FacesContext facesContext, UIComponent uiComponent, List<String> violationMessages)
    {
        if(supportMultipleViolationsPerField())
        {
            boolean firstMessage = false;
            for(String message : violationMessages)
            {
                if(!firstMessage)
                {
                    firstMessage = true;
                }
                else
                {
                    facesContext.addMessage(uiComponent.getClientId(facesContext),
                            new FacesMessage(FacesMessage.SEVERITY_ERROR, message, message));
                }
            }
        }

        throw new ValidatorException(
                new FacesMessage(FacesMessage.SEVERITY_ERROR, violationMessages.get(0), violationMessages.get(0)));
    }

    //override this method in the jsf 2.0 version
    protected boolean supportMultipleViolationsPerField()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.ACTIVATE_MULTIPLE_VIOLATIONS_PER_FIELD);
    }
}
