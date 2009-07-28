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
package org.apache.myfaces.extensions.validator.beanval.interceptor;

import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.ValidationModuleAware;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.beanval.storage.ModelValidationEntry;
import org.apache.myfaces.extensions.validator.beanval.ExtValBeanValidationContext;
import org.apache.myfaces.extensions.validator.beanval.BeanValidationModuleKey;
import org.apache.myfaces.extensions.validator.beanval.annotation.extractor.DefaultGroupControllerScanningExtractor;
import org.apache.myfaces.extensions.validator.beanval.annotation.BeanValidation;
import org.apache.myfaces.extensions.validator.beanval.annotation.ModelValidation;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.validation.groups.Default;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.lang.reflect.Method;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
public class PropertyValidationGroupProvider implements PropertyValidationInterceptor, ValidationModuleAware
{
    protected final Log logger = LogFactory.getLog(getClass());

    public boolean beforeValidation(FacesContext facesContext,
                                    UIComponent uiComponent,
                                    Object convertedObject,
                                    Map<String, Object> properties)
    {
        if(properties.containsKey(PropertyInformation.class.getName()))
        {
            addMetaDataToContext(
                    (PropertyInformation)properties.get(PropertyInformation.class.getName()), uiComponent);
        }
        return true;
    }

    public void afterValidation(FacesContext facesContext,
                                UIComponent uiComponent,
                                Object convertedObject,
                                Map<String, Object> properties)
    {
        //not used
    }

    protected void addMetaDataToContext(PropertyInformation propertyInformation, UIComponent component)
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

        processFoundGroups(extValBeanValidationContext, currentViewId, clientId,
                foundGroupsForPropertyValidation);

        processRestrictedGroups(extValBeanValidationContext, currentViewId, clientId,
                restrictedGroupsForPropertyValidation);

        initModelValidation(extValBeanValidationContext, currentViewId, component, propertyDetails,
                modelValidationEntryList, restrictedGroupsForModelValidation);
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

    protected void processFoundGroups(ExtValBeanValidationContext extValBeanValidationContext,
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

    protected void processRestrictedGroups(ExtValBeanValidationContext extValBeanValidationContext,
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

    protected void initModelValidation(ExtValBeanValidationContext extValBeanValidationContext,
                                     String currentViewId,
                                     UIComponent component,
                                     PropertyDetails propertyDetails,
                                     List<ModelValidationEntry> modelValidationEntryList,
                                     List<Class> restrictedGroupsForModelValidation)
    {
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

            if(modelValidationEntry.getGroups().length > 0)
            {
                addTargetsForModelValidation(modelValidationEntry, propertyDetails.getBaseObject());
                extValBeanValidationContext.addModelValidationEntry(modelValidationEntry, currentViewId, component);
            }
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

    protected void processMetaData(BeanValidation beanValidation,
                                 Object metaDataSourceObject,
                                 List<Class> foundGroupsForPropertyValidation,
                                 List<Class> restrictedGroupsForPropertyValidation,
                                 List<ModelValidationEntry> modelValidationEntryList,
                                 List<Class> restrictedGroupsForModelValidation)
    {
        for (String currentViewId : beanValidation.viewIds())
        {
            if ((currentViewId.equals(FacesContext.getCurrentInstance().getViewRoot().getViewId()) ||
                    currentViewId.equals("*")) && isValidationPermitted(beanValidation))
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

    private boolean isValidationPermitted(BeanValidation beanValidation)
    {
        ELHelper elHelper = ExtValUtils.getELHelper();

        for(String condition : beanValidation.conditions())
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

        if(beanValidation.restrictGroups().length > 0)
        {
            restrictedGroupsForModelValidation.addAll(Arrays.asList(beanValidation.restrictGroups()));
        }

        modelValidationEntryList.add(modelValidationEntry);
    }

    private void processGroups(BeanValidation beanValidation,
                           List<Class> foundGroupsForPropertyValidation,
                           List<Class> restrictedGroupsForPropertyValidation)
    {
        foundGroupsForPropertyValidation.addAll(Arrays.asList(beanValidation.useGroups()));

        if(beanValidation.restrictGroups().length > 0)
        {
            restrictedGroupsForPropertyValidation.addAll(Arrays.asList(beanValidation.restrictGroups()));
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

    public String[] getModuleKeys()
    {
        return new String[] {BeanValidationModuleKey.class.getName()};
    }
}
