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
import org.apache.myfaces.extensions.validator.beanval.ExtValBeanValidationContext;
import org.apache.myfaces.extensions.validator.beanval.annotation.BeanValidation;
import org.apache.myfaces.extensions.validator.beanval.annotation.ModelValidation;
import org.apache.myfaces.extensions.validator.beanval.annotation.extractor.DefaultGroupControllerScanningExtractor;
import org.apache.myfaces.extensions.validator.beanval.payload.ViolationSeverity;
import org.apache.myfaces.extensions.validator.beanval.storage.ModelValidationEntry;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.validation.message.FacesMessageHolder;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.validation.ConstraintViolation;
import javax.validation.Payload;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
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
    private static LabeledMessageInternals labeledMessageInternals = new LabeledMessageInternals();

    @ToDo(value = Priority.HIGH, description = "use it - also in ModelValidationPhaseListener" +
            "attention: only add one message per client id")
    public static boolean supportMultipleViolationsPerField()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.ACTIVATE_MULTIPLE_VIOLATION_MESSAGES_PER_FIELD);
    }

    public static void addMetaDataToContext(UIComponent component, PropertyDetails propertyDetails)
    {
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

        processFoundGroups(extValBeanValidationContext, currentViewId, clientId,
                foundGroupsForPropertyValidation);

        processRestrictedGroups(extValBeanValidationContext, currentViewId, clientId,
                restrictedGroupsForPropertyValidation);

        initModelValidation(extValBeanValidationContext, component, propertyDetails,
                modelValidationEntryList, restrictedGroupsForModelValidation);
    }

    private static void processClass(Object objectToInspect,
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

    private static void processFieldsAndProperties(String key,
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
                tryToProcessMetaData((BeanValidation) metaDataEntry.getValue(),
                        base,
                        foundGroupsForPropertyValidation,
                        restrictedGroupsForPropertyValidation,
                        modelValidationEntryList,
                        restrictedGroupsForModelValidation);
            }
            else if (metaDataEntry.getValue() instanceof BeanValidation.List)
            {
                for (BeanValidation currentBeanValidation : ((BeanValidation.List) metaDataEntry.getValue()).value())
                {
                    tryToProcessMetaData(currentBeanValidation,
                            base,
                            foundGroupsForPropertyValidation,
                            restrictedGroupsForPropertyValidation,
                            modelValidationEntryList,
                            restrictedGroupsForModelValidation);
                }
            }
        }
    }

    private static void processFoundGroups(ExtValBeanValidationContext extValBeanValidationContext,
                                           String currentViewId,
                                           String clientId,
                                           List<Class> foundGroupsForPropertyValidation)
    {
        /*
         * add found groups to context
         */
        for (Class currentGroupClass : foundGroupsForPropertyValidation)
        {
            extValBeanValidationContext.addGroup(currentGroupClass, currentViewId, clientId);
        }
    }

    private static void processRestrictedGroups(ExtValBeanValidationContext extValBeanValidationContext,
                                                String currentViewId,
                                                String clientId,
                                                List<Class> restrictedGroupsForPropertyValidation)
    {
        /*
         * add restricted groups
         */
        for (Class currentGroupClass : restrictedGroupsForPropertyValidation)
        {
            extValBeanValidationContext.restrictGroup(currentGroupClass, currentViewId, clientId);
        }
    }

    private static void initModelValidation(ExtValBeanValidationContext extValBeanValidationContext,
                                            UIComponent component,
                                            PropertyDetails propertyDetails,
                                            List<ModelValidationEntry> modelValidationEntryList,
                                            List<Class> restrictedGroupsForModelValidation)
    {
        /*
         * add model validation entry list
         */
        for (ModelValidationEntry modelValidationEntry : modelValidationEntryList)
        {
            for (Class restrictedGroup : restrictedGroupsForModelValidation)
            {
                modelValidationEntry.removeGroup(restrictedGroup);
            }

            if (modelValidationEntry.getGroups().length > 0)
            {
                addTargetsForModelValidation(modelValidationEntry, propertyDetails.getBaseObject());
                modelValidationEntry.setComponent(component);
                extValBeanValidationContext.addModelValidationEntry(modelValidationEntry);
            }
        }
    }

    private static void transferGroupValidationInformationToFoundGroups(
            Object objectToInspect,
            List<Class> foundGroupsForPropertyValidation,
            List<Class> restrictedGroupsForPropertyValidation,
            List<ModelValidationEntry> modelValidationEntryList,
            List<Class> restrictedGroupsForModelValidation)
    {
        if (objectToInspect.getClass().isAnnotationPresent(BeanValidation.class))
        {
            tryToProcessMetaData(objectToInspect.getClass().getAnnotation(BeanValidation.class),
                    objectToInspect,
                    foundGroupsForPropertyValidation,
                    restrictedGroupsForPropertyValidation,
                    modelValidationEntryList,
                    restrictedGroupsForModelValidation);
        }
        else if (objectToInspect.getClass().isAnnotationPresent(BeanValidation.List.class))
        {
            for (BeanValidation currentBeanValidation :
                    (objectToInspect.getClass().getAnnotation(BeanValidation.List.class)).value())
            {
                tryToProcessMetaData(currentBeanValidation,
                        objectToInspect,
                        foundGroupsForPropertyValidation,
                        restrictedGroupsForPropertyValidation,
                        modelValidationEntryList,
                        restrictedGroupsForModelValidation);
            }
        }
    }

    private static void processInterfaces(Class currentClass,
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

    private static void tryToProcessMetaData(BeanValidation beanValidation,
                                        Object metaDataSourceObject,
                                        List<Class> foundGroupsForPropertyValidation,
                                        List<Class> restrictedGroupsForPropertyValidation,
                                        List<ModelValidationEntry> modelValidationEntryList,
                                        List<Class> restrictedGroupsForModelValidation)
    {
        for (String currentViewId : beanValidation.viewIds())
        {
            if (useMetaDataForViewId(beanValidation, currentViewId))
            {
                processMetaData(beanValidation,
                        metaDataSourceObject,
                        foundGroupsForPropertyValidation,
                        restrictedGroupsForPropertyValidation,
                        modelValidationEntryList,
                        restrictedGroupsForModelValidation);
                break;
            }
        }
    }

    private static boolean useMetaDataForViewId(BeanValidation beanValidation, String currentViewId)
    {
        return (currentViewId.equals(FacesContext.getCurrentInstance().getViewRoot().getViewId()) ||
                currentViewId.equals("*")) && isValidationPermitted(beanValidation);
    }

    private static void processMetaData(BeanValidation beanValidation,
                                                 Object metaDataSourceObject,
                                                 List<Class> foundGroupsForPropertyValidation,
                                                 List<Class> restrictedGroupsForPropertyValidation,
                                                 List<ModelValidationEntry> modelValidationEntryList,
                                                 List<Class> restrictedGroupsForModelValidation)
    {
        if (isModelValidation(beanValidation))
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
    }

    private static void addTargetsForModelValidation(ModelValidationEntry modelValidationEntry, Object defaultTarget)
    {
        if (isDefaultTarget(modelValidationEntry))
        {
            modelValidationEntry.addValidationTarget(defaultTarget);
        }
        else
        {
            addValidationTargets(modelValidationEntry);
        }
    }

    private static boolean isDefaultTarget(ModelValidationEntry modelValidationEntry)
    {
        return modelValidationEntry.getValidationTargetExpressions().size() == 1 &&
                modelValidationEntry.getValidationTargetExpressions().iterator().next()
                        .equals(ModelValidation.DEFAULT_TARGET);
    }

    private static void addValidationTargets(ModelValidationEntry modelValidationEntry)
    {
        Object target;
        for (String modelValidationTarget : modelValidationEntry.getValidationTargetExpressions())
        {
            target = resolveTarget(modelValidationEntry.getMetaDataSourceObject(), modelValidationTarget);

            if (target == null && LOG.isErrorEnabled())
            {
                LOG.error("target unreachable - source class: " +
                        modelValidationEntry.getMetaDataSourceObject().getClass().getName() +
                        " target to resolve: " + modelValidationTarget);
            }

            modelValidationEntry.addValidationTarget(target);
        }
    }

    private static boolean isValidationPermitted(BeanValidation beanValidation)
    {
        ELHelper elHelper = ExtValUtils.getELHelper();

        for (String condition : beanValidation.conditions())
        {
            if (elHelper.isELTermWellFormed(condition) &&
                    elHelper.isELTermValid(FacesContext.getCurrentInstance(), condition))
            {
                if (Boolean.TRUE.equals(
                        elHelper.getValueOfExpression(
                                FacesContext.getCurrentInstance(), new ValueBindingExpression(condition))))
                {
                    return true;
                }
            }
            else
            {
                if (LOG.isErrorEnabled())
                {
                    LOG.error("an invalid condition is used: " + condition);
                }
            }
        }
        return false;
    }

    private static boolean isModelValidation(BeanValidation beanValidation)
    {
        return beanValidation.modelValidation().isActive();
    }

    private static void addModelValidationEntry(BeanValidation beanValidation,
                                                Object metaDataSourceObject,
                                                List<ModelValidationEntry> modelValidationEntryList,
                                                List<Class> restrictedGroupsForModelValidation)
    {
        ModelValidationEntry modelValidationEntry = new ModelValidationEntry();

        modelValidationEntry.setGroups(Arrays.asList(beanValidation.useGroups()));
        modelValidationEntry.setDisplayMessageInline(beanValidation.modelValidation().displayInline());
        modelValidationEntry.setCustomMessage(beanValidation.modelValidation().message());
        modelValidationEntry.setMetaDataSourceObject(metaDataSourceObject);

        for(String validationTargetExpression : beanValidation.modelValidation().validationTargets())
        {
            modelValidationEntry.addValidationTargetExpression(validationTargetExpression);
        }

        if (beanValidation.restrictGroups().length > 0)
        {
            restrictedGroupsForModelValidation.addAll(Arrays.asList(beanValidation.restrictGroups()));
        }

        modelValidationEntryList.add(modelValidationEntry);
    }

    private static void processGroups(BeanValidation beanValidation,
                                      List<Class> foundGroupsForPropertyValidation,
                                      List<Class> restrictedGroupsForPropertyValidation)
    {
        foundGroupsForPropertyValidation.addAll(Arrays.asList(beanValidation.useGroups()));

        if (beanValidation.restrictGroups().length > 0)
        {
            restrictedGroupsForPropertyValidation.addAll(Arrays.asList(beanValidation.restrictGroups()));
        }
    }

    private static Object resolveTarget(Object metaDataSourceObject, String modelValidationTarget)
    {
        ELHelper elHelper = ExtValUtils.getELHelper();

        if (elHelper.isELTermWellFormed(modelValidationTarget))
        {
            if (elHelper.isELTermValid(FacesContext.getCurrentInstance(), modelValidationTarget))
            {
                return elHelper.getValueOfExpression(
                        FacesContext.getCurrentInstance(), new ValueBindingExpression(modelValidationTarget));
            }
            else
            {
                if (LOG.isErrorEnabled())
                {
                    LOG.error("an invalid binding is used: " + modelValidationTarget);
                }
            }
        }

        String[] properties = modelValidationTarget.split("\\.");

        Object result = metaDataSourceObject;
        for (String property : properties)
        {
            result = getValueOfProperty(result, property);

            if (result == null)
            {
                return null;
            }
        }

        return result;
    }

    private static Object getValueOfProperty(Object base, String property)
    {
        property = property.substring(0, 1).toUpperCase() + property.substring(1, property.length());
        Method targetMethod = ReflectionUtils.tryToGetMethod(base.getClass(), "get" + property);

        if (targetMethod == null)
        {
            targetMethod = ReflectionUtils.tryToGetMethod(base.getClass(), "is" + property);
        }

        if (targetMethod == null)
        {
            throw new IllegalStateException(
                    "class " + base.getClass() + " has no public get/is " + property.toLowerCase());
        }
        return ReflectionUtils.tryToInvokeMethod(base, targetMethod);
    }

    public static void processConstraintViolations(FacesContext facesContext,
                                                   UIComponent uiComponent,
                                                   Object convertedObject,
                                                   Set<ConstraintViolation> violations,
                                                   boolean firstErrorCausesAnException)
    {
        List<FacesMessageHolder> facesMessageHolderList = new ArrayList<FacesMessageHolder>();

        FacesMessage facesMessage;
        for (ConstraintViolation violation : violations)
        {
            facesMessage = createFacesMessageForConstraintViolation(uiComponent, convertedObject, violation);

            if(facesMessage == null)
            {
                continue;
            }

            processFacesMessage(facesContext, uiComponent, facesMessageHolderList, facesMessage);
        }

        processViolationMessages(facesMessageHolderList, firstErrorCausesAnException);
    }

    @ToDo(value = Priority.HIGH, description = "support severity.warn and .info")
    public static FacesMessage createFacesMessageForConstraintViolation(UIComponent uiComponent,
                                                                        Object convertedObject,
                                                                        ConstraintViolation violation)
    {
        String violationMessage = violation.getMessage();

        String labeledMessage = createLabeledMessage(violationMessage);

        FacesMessage.Severity severity = calcSeverity(violation);

        ValidatorException validatorException = createValidatorException(labeledMessage, severity);

        if(!executeAfterThrowingInterceptors(uiComponent, convertedObject, validatorException))
        {
            return null;
        }

        if (isMessageTextUnchanged(validatorException, labeledMessage))
        {
            return ExtValUtils.createFacesMessage(severity, violationMessage, violationMessage);
        }
        else
        {
            String newMessage = validatorException.getFacesMessage().getSummary();
            return ExtValUtils.createFacesMessage(severity, newMessage, newMessage);
        }
    }

    private static String createLabeledMessage(String violationMessage)
    {
        return labeledMessageInternals.createLabeledMessage(violationMessage);
    }

    private static FacesMessage.Severity calcSeverity(ConstraintViolation<?> violation)
    {
        for (Class<? extends Payload> payload : violation.getConstraintDescriptor().getPayload())
        {
            if (ViolationSeverity.Warn.class.isAssignableFrom(payload))
            {
                return FacesMessage.SEVERITY_WARN;
            }
            else if (ViolationSeverity.Info.class.isAssignableFrom(payload))
            {
                return FacesMessage.SEVERITY_INFO;
            }
            else if (ViolationSeverity.Fatal.class.isAssignableFrom(payload))
            {
                return FacesMessage.SEVERITY_FATAL;
            }
        }
        return FacesMessage.SEVERITY_ERROR;
    }

    private static void processFacesMessage(FacesContext facesContext, UIComponent uiComponent,
                                            List<FacesMessageHolder> facesMessageHolderList, FacesMessage facesMessage)
    {
        FacesMessageHolder facesMessageHolder = new FacesMessageHolder(facesMessage);

        facesMessageHolder.setClientId(uiComponent.getClientId(facesContext));

        facesMessageHolderList.add(facesMessageHolder);
    }

    private static boolean executeAfterThrowingInterceptors(UIComponent uiComponent,
                                                         Object convertedObject,
                                                         ValidatorException validatorException)
    {
        return ExtValUtils.executeAfterThrowingInterceptors(
                uiComponent,
                null,
                convertedObject,
                validatorException,
                null);
    }

    private static boolean isMessageTextUnchanged(ValidatorException validatorException, String violationMessage)
    {
        return violationMessage.equals(validatorException.getFacesMessage().getSummary()) ||
                violationMessage.equals(validatorException.getFacesMessage().getDetail());
    }

    private static ValidatorException createValidatorException(String violationMessage, FacesMessage.Severity severity)
    {
        return new ValidatorException(
                ExtValUtils.createFacesMessage(severity, violationMessage, violationMessage));
    }

    public static void processViolationMessages(List<FacesMessageHolder> violationMessageHolderList,
                                                boolean firstErrorCausesAnException)
    {
        if(violationMessageHolderList == null || violationMessageHolderList.isEmpty())
        {
            return;
        }

        List<FacesMessageHolder> facesMessageListWithLowSeverity =
                getFacesMessageListWithLowSeverity(violationMessageHolderList);
        List<FacesMessageHolder> facesMessageListWithHighSeverity =
                getFacesMessageListWithHighSeverity(violationMessageHolderList);

        addMessagesWithHighSeverity(facesMessageListWithHighSeverity, firstErrorCausesAnException);
        addMessagesWithLowSeverity(facesMessageListWithLowSeverity);

        if (!facesMessageListWithHighSeverity.isEmpty() && firstErrorCausesAnException)
        {
            FacesMessageHolder facesMessageHolder = facesMessageListWithHighSeverity.iterator().next();
            ExtValUtils.tryToThrowValidatorExceptionForComponentId(
                    facesMessageHolder.getClientId(), facesMessageHolder.getFacesMessage(), null);
        }
    }

    private static List<FacesMessageHolder> getFacesMessageListWithLowSeverity(
            List<FacesMessageHolder> violationMessages)
    {
        List<FacesMessageHolder> result = new ArrayList<FacesMessageHolder>();

        for (FacesMessageHolder facesMessageHolder : violationMessages)
        {
            if (FacesMessage.SEVERITY_WARN.equals(facesMessageHolder.getFacesMessage().getSeverity()) ||
                    FacesMessage.SEVERITY_INFO.equals(facesMessageHolder.getFacesMessage().getSeverity()))
            {
                result.add(facesMessageHolder);
            }
        }
        return result;
    }

    private static List<FacesMessageHolder> getFacesMessageListWithHighSeverity(
            List<FacesMessageHolder> violationMessageHolderList)
    {
        List<FacesMessageHolder> result = new ArrayList<FacesMessageHolder>();

        for (FacesMessageHolder facesMessageHolder : violationMessageHolderList)
        {
            if (FacesMessage.SEVERITY_ERROR.equals(facesMessageHolder.getFacesMessage().getSeverity()) ||
                    FacesMessage.SEVERITY_FATAL.equals(facesMessageHolder.getFacesMessage().getSeverity()))
            {
                result.add(facesMessageHolder);
            }
        }
        return result;
    }

    private static void addMessagesWithHighSeverity(List<FacesMessageHolder> facesMessageHolderListWithHighSeverity,
                                                    boolean firstErrorCausesAnException)
    {
        boolean firstMessage = true;
        for (FacesMessageHolder facesMessageHolder : facesMessageHolderListWithHighSeverity)
        {
            if (firstMessage && firstErrorCausesAnException)
            {
                //the first error will be thrown as exception
                firstMessage = false;
            }
            else
            {
                ExtValUtils.tryToAddViolationMessageForComponentId(
                        facesMessageHolder.getClientId(), facesMessageHolder.getFacesMessage());
            }
        }
    }

    private static void addMessagesWithLowSeverity(List<FacesMessageHolder> facesMessageHolderListWithLowSeverity)
    {
        for (FacesMessageHolder facesMessageHolder : facesMessageHolderListWithLowSeverity)
        {
            ExtValUtils.tryToAddViolationMessageForComponentId(
                    facesMessageHolder.getClientId(), facesMessageHolder.getFacesMessage());
        }
    }
}
