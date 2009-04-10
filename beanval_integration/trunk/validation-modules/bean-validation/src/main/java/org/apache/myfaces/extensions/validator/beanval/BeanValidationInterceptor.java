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
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.interceptor.AbstractRendererInterceptor;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.beanval.property.BeanValidationPropertyInformationKeys;
import org.apache.myfaces.extensions.validator.beanval.annotation.BeanValidation;
import org.apache.myfaces.extensions.validator.beanval.annotation.ModelValidation;
import org.apache.myfaces.extensions.validator.beanval.annotation.extractor.DefaultGroupControllerScanningExtractor;
import org.apache.myfaces.extensions.validator.beanval.validation.strategy.BeanValidationStrategyAdapter;
import org.apache.myfaces.extensions.validator.beanval.validation.ModelValidationEntry;
import org.apache.myfaces.extensions.validator.beanval.annotation.NoRestrictionGroup;
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
import javax.validation.BeanDescriptor;
import javax.validation.ElementDescriptor;
import javax.validation.ConstraintDescriptor;
import javax.validation.ValidatorFactory;
import javax.validation.ConstraintViolation;
import javax.validation.groups.Default;
import java.util.Set;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.IOException;
import java.lang.reflect.Method;

/**
 * @author Gerhard Petracek
 * @since 1.x.3
 */
public class BeanValidationInterceptor extends AbstractRendererInterceptor
{
    private ValidatorFactory validationFactory = Validation.buildDefaultValidatorFactory();

    @Override
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        initComponent(facesContext, uiComponent);
    }

    @ToDo(value = Priority.HIGH, description = "check groups")
    protected void initComponent(FacesContext facesContext, UIComponent uiComponent)
    {
        if (!(uiComponent instanceof EditableValueHolder))
        {
            return;
        }

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

        for (ConstraintDescriptor<?> constraintDescriptor : elementDescriptor.getConstraintDescriptors())
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
    public void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
            throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        Object convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);

        //recorde user input e.g. for cross-component validation
        for (ProcessedInformationRecorder recorder : ExtValContext.getContext().getProcessedInformationRecorders())
        {
            recorder.recordUserInput(uiComponent, convertedObject);

            if (logger.isTraceEnabled())
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

    @ToDo(value = Priority.HIGH, description = "use ExtValUtils#createFacesMessage")
    protected void processValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject)
    {
        if (!(uiComponent instanceof EditableValueHolder))
        {
            return;
        }

        if (logger.isTraceEnabled())
        {
            logger.trace("jsr303 start validation");
        }

        if (!"true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_EMPTY_STRING_INTERPRETATION)
                && "".equals(convertedObject))
        {
            convertedObject = null;
        }

        MetaDataExtractor metaDataExtractor = ExtValUtils.getComponentMetaDataExtractor();

        PropertyInformation propertyInformation = metaDataExtractor.extract(facesContext, uiComponent);

        //extract group validation annotations
        addMetaDataToContext(propertyInformation, uiComponent);

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

        if (violations != null && violations.size() > 0)
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

            //TODO check & remove
            propertyInformation.setInformation(
                    BeanValidationPropertyInformationKeys.CONSTRAINT_VIOLATIONS, violations);

            if (labeledMessage.equals(validatorException.getFacesMessage().getSummary()) ||
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

        if (logger.isTraceEnabled())
        {
            logger.trace("jsr303 validation finished");
        }
    }

    private void addMetaDataToContext(PropertyInformation propertyInformation, UIComponent component)
    {
        PropertyDetails propertyDetails = propertyInformation
                .getInformation(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        String[] key = propertyDetails.getKey().split("\\.");

        Object firstBean = ExtValUtils.getELHelper().getBean(key[0]);

        List<Class> foundGroupsForPropertyValidation = new ArrayList<Class>();
        List<Class> restrictedGroupsForPropertyValidation = new ArrayList<Class>();
        List<ModelValidationEntry> modelValidationEntryList = new ArrayList<ModelValidationEntry>();
        List<Class> restrictedGroupsForModelValidation = new ArrayList<Class>();

        //extract bv-controller-annotation of

        //first bean
        processClass(firstBean,
                foundGroupsForPropertyValidation,
                restrictedGroupsForPropertyValidation,
                modelValidationEntryList,
                restrictedGroupsForModelValidation);

        //first property
        processFieldsAndProperties(key[0] + "." + key[1],
                firstBean,
                key[1],
                foundGroupsForPropertyValidation,
                restrictedGroupsForPropertyValidation,
                modelValidationEntryList,
                restrictedGroupsForModelValidation);

        //base object (of target property)
        processClass(propertyDetails.getBaseObject(),
                foundGroupsForPropertyValidation,
                restrictedGroupsForPropertyValidation,
                modelValidationEntryList,
                restrictedGroupsForModelValidation);

        //last property
        processFieldsAndProperties(
                propertyDetails.getKey(),
                propertyDetails.getBaseObject(),
                propertyDetails.getProperty(),
                foundGroupsForPropertyValidation,
                restrictedGroupsForPropertyValidation,
                modelValidationEntryList,
                restrictedGroupsForModelValidation);

        ExtValBeanValidationContext extValBeanValidationContext = ExtValBeanValidationContext.getCurrentInstance();
        String currentViewId = FacesContext.getCurrentInstance().getViewRoot().getViewId();

        String clientId = component.getClientId(FacesContext.getCurrentInstance());
        /*
         * add found groups to context
         */
        for (Class currentGroupClass : foundGroupsForPropertyValidation)
        {
            extValBeanValidationContext.addGroup(currentGroupClass, currentViewId, clientId);
        }

        /*
         * add restricted groups
         */
        for (Class currentGroupClass : restrictedGroupsForPropertyValidation)
        {
            extValBeanValidationContext.restrictGroup(currentGroupClass, currentViewId, clientId);
        }

        /*
         * add model validation entry list
         */
        for(ModelValidationEntry modelValidationEntry : modelValidationEntryList)
        {
            if(!"true".equalsIgnoreCase(org.apache.myfaces.extensions.validator.beanval.WebXmlParameter
                    .DEACTIVATE_IMPLICIT_DEFAULT_GROUP_VALIDATION))
            {
                modelValidationEntry.addGroup(Default.class);
            }

            for(Class restrictedGroup : restrictedGroupsForModelValidation)
            {
                modelValidationEntry.removeGroup(restrictedGroup);
            }

            if(modelValidationEntry.getGroups().length > 0 &&
                    isModelValidationAllowed(modelValidationEntry.getMetaData()))
            {
                addTargetsForModelValidation(modelValidationEntry, propertyDetails.getBaseObject());
                extValBeanValidationContext.addModelValidationEntry(modelValidationEntry, currentViewId, component);
            }
        }
    }

    private void processClass(Object objectToInspect,
                              List<Class> foundGroupsForPropertyValidation,
                              List<Class> restrictedGroupsForPropertyValidation,
                              List<ModelValidationEntry> modelValidationEntryList,
                              List<Class> restrictedGroupsForModelValidation)
    {
        Class classToInspect = objectToInspect.getClass();
        while (!Object.class.getName().equals(classToInspect.getName()))
        {
            transferGroupValidationInformationToFoundGroups(objectToInspect,
                    foundGroupsForPropertyValidation,
                    restrictedGroupsForPropertyValidation,
                    modelValidationEntryList,
                    restrictedGroupsForModelValidation);

            processInterfaces(objectToInspect.getClass(), objectToInspect,
                    foundGroupsForPropertyValidation,
                    restrictedGroupsForPropertyValidation,
                    modelValidationEntryList,
                    restrictedGroupsForModelValidation);

            classToInspect = classToInspect.getSuperclass();
        }
    }

    private void processInterfaces(Class currentClass,
                                   Object metaDataSourceObject,
                                   List<Class> foundGroupsForPropertyValidation,
                                   List<Class> restrictedGroupsForPropertyValidation,
                                   List<ModelValidationEntry> modelValidationEntryList,
                                   List<Class> restrictedGroupsForModelValidation)
    {
        for (Class currentInterface : currentClass.getInterfaces())
        {
            transferGroupValidationInformationToFoundGroups(metaDataSourceObject,
                    foundGroupsForPropertyValidation,
                    restrictedGroupsForPropertyValidation,
                    modelValidationEntryList,
                    restrictedGroupsForModelValidation);

            processInterfaces(currentInterface, metaDataSourceObject,
                    foundGroupsForPropertyValidation,
                    restrictedGroupsForPropertyValidation,
                    modelValidationEntryList,
                    restrictedGroupsForModelValidation);
        }
    }

    private void transferGroupValidationInformationToFoundGroups(Object objectToInspect,
                                                                 List<Class> foundGroupsForPropertyValidation,
                                                                 List<Class> restrictedGroupsForPropertyValidation,
                                                                 List<ModelValidationEntry> modelValidationEntryList,
                                                                 List<Class> restrictedGroupsForModelValidation)
    {
        if (objectToInspect.getClass().isAnnotationPresent(BeanValidation.class))
        {
            processMetaData(objectToInspect.getClass().getAnnotation(BeanValidation.class),
                    objectToInspect,
                    foundGroupsForPropertyValidation,
                    restrictedGroupsForPropertyValidation,
                    modelValidationEntryList,
                    restrictedGroupsForModelValidation);
        }
        else if (objectToInspect.getClass().isAnnotationPresent(BeanValidation.List.class))
        {
            for(BeanValidation currentBeanValidation :
                    (objectToInspect.getClass().getAnnotation(BeanValidation.List.class)).value())
            {
                processMetaData(currentBeanValidation,
                        objectToInspect,
                        foundGroupsForPropertyValidation,
                        restrictedGroupsForPropertyValidation,
                        modelValidationEntryList,
                        restrictedGroupsForModelValidation);
            }
        }
    }

    private void processFieldsAndProperties(String key,
                                            Object base,
                                            String property, List<Class> foundGroupsForPropertyValidation,
                                            List<Class> restrictedGroupsForPropertyValidation,
                                            List<ModelValidationEntry> modelValidationEntryList,
                                            List<Class> restrictedGroupsForModelValidation)
    {
        PropertyInformation propertyInformation = new DefaultGroupControllerScanningExtractor()
                .extract(FacesContext.getCurrentInstance(), new PropertyDetails(key, base, property));

        for (MetaDataEntry metaDataEntry : propertyInformation.getMetaDataEntries())
        {
            if (metaDataEntry.getValue() instanceof BeanValidation)
            {
                processMetaData((BeanValidation) metaDataEntry.getValue(),
                        base,
                        foundGroupsForPropertyValidation,
                        restrictedGroupsForPropertyValidation,
                        modelValidationEntryList,
                        restrictedGroupsForModelValidation);
            }
            else if(metaDataEntry.getValue() instanceof BeanValidation.List)
            {
                for(BeanValidation currentBeanValidation : ((BeanValidation.List)metaDataEntry.getValue()).value())
                {
                    processMetaData(currentBeanValidation,
                            base,
                            foundGroupsForPropertyValidation,
                            restrictedGroupsForPropertyValidation,
                            modelValidationEntryList,
                            restrictedGroupsForModelValidation);
                }
            }
        }
    }

    private void processMetaData(BeanValidation beanValidation,
                                 Object metaDataSourceObject,
                                 List<Class> foundGroupsForPropertyValidation,
                                 List<Class> restrictedGroupsForPropertyValidation,
                                 List<ModelValidationEntry> modelValidationEntryList,
                                 List<Class> restrictedGroupsForModelValidation)
    {
        for (String currentViewId : beanValidation.viewIds())
        {
            if (currentViewId.equals(FacesContext.getCurrentInstance().getViewRoot().getViewId()) ||
                    currentViewId.equals("*"))
            {
                if(isModelValidation(beanValidation))
                {
                    addModelValidationEntry(
                            beanValidation, metaDataSourceObject,
                            modelValidationEntryList, restrictedGroupsForModelValidation);
                }
                else
                {
                    processGroups(
                            beanValidation, foundGroupsForPropertyValidation, restrictedGroupsForPropertyValidation);
                }

                return;
            }
        }
    }

    private void processGroups(BeanValidation beanValidation,
                           List<Class> foundGroupsForPropertyValidation,
                           List<Class> restrictedGroupsForPropertyValidation)
    {
        foundGroupsForPropertyValidation.addAll(Arrays.asList(beanValidation.useGroups()));

        if(!(beanValidation.restrictGroups().length == 1 &&
                beanValidation.restrictGroups()[0].equals(NoRestrictionGroup.class)))
        {
            restrictedGroupsForPropertyValidation.addAll(Arrays.asList(beanValidation.restrictGroups()));
        }
    }

    private boolean isModelValidation(BeanValidation beanValidation)
    {
        return beanValidation.modelValidation().isActive();
    }

    private void addModelValidationEntry(BeanValidation beanValidation,
                                         Object metaDataSourceObject,
                                         List<ModelValidationEntry> modelValidationEntryList,
                                         List<Class> restrictedGroupsForModelValidation)
    {
        ModelValidationEntry modelValidationEntry = new ModelValidationEntry();

        modelValidationEntry.setGroups(Arrays.asList(beanValidation.useGroups()));
        modelValidationEntry.setMetaData(beanValidation.modelValidation());
        modelValidationEntry.setMetaDataSourceObject(metaDataSourceObject);

        if(!(beanValidation.restrictGroups().length == 1 &&
                beanValidation.restrictGroups()[0].equals(NoRestrictionGroup.class)))
        {
            restrictedGroupsForModelValidation.addAll(Arrays.asList(beanValidation.restrictGroups()));
        }

        modelValidationEntryList.add(modelValidationEntry);
    }

    private void addTargetsForModelValidation(ModelValidationEntry modelValidationEntry, Object defaultTarget)
    {
        if(modelValidationEntry.getMetaData().validationTargets().length == 1 &&
                modelValidationEntry.getMetaData().validationTargets()[0].equals(ModelValidation.DEFAULT_TARGET))
        {
            modelValidationEntry.addValidationTarget(defaultTarget);
        }
        else
        {
            Object target;
            for(String modelValidationTarget : modelValidationEntry.getMetaData().validationTargets())
            {
                target = resolveTarget(modelValidationEntry.getMetaDataSourceObject(), modelValidationTarget);

                if(target == null && this.logger.isErrorEnabled())
                {
                    this.logger.error("target unreachable - source class: " +
                            modelValidationEntry.getMetaDataSourceObject().getClass().getName() +
                            " target to resolve: " + modelValidationTarget);
                }

                modelValidationEntry.addValidationTarget(target);
            }
        }
    }

    private Object resolveTarget(Object metaDataSourceObject, String modelValidationTarget)
    {
        ELHelper elHelper = ExtValUtils.getELHelper();

        if(elHelper.isELTermWellFormed(modelValidationTarget))
        {
            if(elHelper.isELTermValid(FacesContext.getCurrentInstance(), modelValidationTarget))
            {
                return elHelper.getValueOfExpression(
                        FacesContext.getCurrentInstance(), new ValueBindingExpression(modelValidationTarget));
            }
            else
            {
                if(this.logger.isErrorEnabled())
                {
                    this.logger.error("an invalid binding is used: " + modelValidationTarget);
                }
            }
        }

        String[] properties = modelValidationTarget.split("\\.");

        Object result = metaDataSourceObject;
        for(String property : properties)
        {
            result = getValueOfProperty(result, property);

            if(result == null)
            {
                return null;
            }
        }

        return result;
    }

    @ToDo(value = Priority.HIGH, description = "move to util class - the original method is in LocalCompareStrategy")
    protected Object getValueOfProperty(Object base, String property)
    {
        property = property.substring(0,1).toUpperCase() + property.substring(1, property.length());
        Method targetMethod = ReflectionUtils.tryToGetMethod(base.getClass(), "get" + property);

        if(targetMethod == null)
        {
            targetMethod = ReflectionUtils.tryToGetMethod(base.getClass(), "is" + property);
        }

        if(targetMethod == null)
        {
            throw new IllegalStateException(
                "class " + base.getClass() + " has no public get/is " + property.toLowerCase());
        }
        return ReflectionUtils.tryToInvokeMethod(base, targetMethod);
    }

    private boolean isModelValidationAllowed(ModelValidation modelValidation)
    {
        ELHelper elHelper = ExtValUtils.getELHelper();

        for(String condition : modelValidation.conditions())
        {
            if(elHelper.isELTermWellFormed(condition) &&
                    elHelper.isELTermValid(FacesContext.getCurrentInstance(), condition))
            {
                if(Boolean.TRUE.equals(
                        elHelper.getValueOfExpression(
                                FacesContext.getCurrentInstance(), new ValueBindingExpression(condition))))
                {
                    return true;
                }
            }
            else
            {
                if(this.logger.isErrorEnabled())
                {
                    this.logger.error("an invalid condition is used: " + condition);
                }
            }
        }
        return false;
    }
}
