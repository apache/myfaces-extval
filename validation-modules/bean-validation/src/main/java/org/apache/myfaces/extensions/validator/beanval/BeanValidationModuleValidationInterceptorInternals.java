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

import org.apache.myfaces.extensions.validator.beanval.validation.strategy.BeanValidationVirtualValidationStrategy;
import org.apache.myfaces.extensions.validator.beanval.util.BeanValidationUtils;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.validation.ConstraintViolation;
import javax.validation.groups.Default;
import javax.validation.metadata.ConstraintDescriptor;
import javax.validation.metadata.ElementDescriptor;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

/**
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
class BeanValidationModuleValidationInterceptorInternals
{
    private Logger logger;

    BeanValidationModuleValidationInterceptorInternals(Logger logger)
    {
        this.logger = logger;
    }

    PropertyDetails extractPropertyDetails(
            FacesContext facesContext, UIComponent uiComponent, Map<String, Object> propertiesForExtraction)
    {
        PropertyDetails result = getComponentMetaDataExtractor(propertiesForExtraction)
                .extract(facesContext, uiComponent)
                .getInformation(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        if (result.getBaseObject() == null)
        {
            this.logger.warning("no base object at " + result.getKey() +
                    " component-id: " + uiComponent.getClientId(facesContext));
        }

        return result.getBaseObject() != null ? result : null;
    }

    /*
     * also invokes meta-data extraction interceptors
     * (see e.g. ExtValBeanValidationMetaDataExtractionInterceptor)
     */
    MetaDataExtractor getComponentMetaDataExtractor(Map<String, Object> properties)
    {
        return ExtValUtils.getComponentMetaDataExtractorWith(properties);
    }

    void initComponentWithPropertyDetails(
            FacesContext facesContext, UIComponent uiComponent, PropertyDetails propertyDetails)
    {
        Class[] foundGroups = resolveGroups(facesContext, uiComponent);

        if (foundGroups == null)
        {
            return;
        }
        else if (foundGroups.length == 0)
        {
            foundGroups = new Class[]{Default.class};
        }

        Class targetClass = propertyDetails.getBaseObject().getClass();

        targetClass = ProxyUtils.getUnproxiedClass(targetClass);

        ElementDescriptor elementDescriptor = BeanValidationUtils.getElementDescriptor(
                targetClass, propertyDetails.getProperty());

        if (elementDescriptor == null)
        {
            return;
        }

        processElementDescriptor(facesContext, uiComponent, foundGroups, elementDescriptor);
    }

    void processElementDescriptor(FacesContext facesContext,
                                  UIComponent uiComponent,
                                  Class[] foundGroups,
                                  ElementDescriptor elementDescriptor)
    {
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

    @ToDo(value = Priority.MEDIUM, description = "ConstraintDescriptor#isReportAsSingleViolation")
    private Map<String, Object> transformConstraintDescriptorToMetaData(
            ConstraintDescriptor<?> constraintDescriptor, Class elementClass)
    {
        Map<String, Object> result = new HashMap<String, Object>();
        MetaDataTransformer metaDataTransformer;

        metaDataTransformer = ExtValUtils.getMetaDataTransformerForValidationStrategy(
                new BeanValidationVirtualValidationStrategy(constraintDescriptor, elementClass));

        if (metaDataTransformer != null)
        {
            result.putAll(transformMetaData(metaDataTransformer, constraintDescriptor));
        }

        if (!constraintDescriptor.isReportAsSingleViolation())
        {
            Set<ConstraintDescriptor<?>> composingConstraints = constraintDescriptor.getComposingConstraints();
            if (composingConstraints != null && !composingConstraints.isEmpty())
            {
                result.putAll(transformComposingConstraints(composingConstraints, elementClass));
            }
        }

        return result;
    }

    private Map<String, Object> transformComposingConstraints(
            Set<ConstraintDescriptor<?>> composingConstraints, Class elementClass)
    {
        Map<String, Object> result = new HashMap<String, Object>();
        for (ConstraintDescriptor constraintDescriptor : composingConstraints)
        {
            result.putAll(transformConstraintDescriptorToMetaData(constraintDescriptor, elementClass));
        }

        return result;
    }

    private Map<String, Object> transformMetaData(
            MetaDataTransformer metaDataTransformer, ConstraintDescriptor<?> constraintDescriptor)
    {
        MetaDataEntry entry;
        Map<String, Object> result;
        
        this.logger.fine(metaDataTransformer.getClass().getName() + " instantiated");

        entry = new MetaDataEntry();
        entry.setKey(constraintDescriptor.getAnnotation().annotationType().getName());
        entry.setValue(constraintDescriptor);

        result = metaDataTransformer.convertMetaData(entry);
        return result;
    }

    boolean hasBeanValidationConstraints(PropertyInformation propertyInformation)
    {
        PropertyDetails propertyDetails = ExtValUtils.getPropertyDetails(propertyInformation);

        Class targetClass = ProxyUtils.getUnproxiedClass(propertyDetails.getBaseObject().getClass());

        return BeanValidationUtils.getElementDescriptor(targetClass, propertyDetails.getProperty()) != null;
    }

    @SuppressWarnings({"unchecked"})
    Set<ConstraintViolation<Object>> validate(FacesContext facesContext,
                                              UIComponent uiComponent,
                                              Object convertedObject,
                                              PropertyInformation propertyInformation,
                                              boolean cascadedValidation)
    {
        Class baseBeanClass = getBaseClassType(propertyInformation);
        String propertyName = getPropertyToValidate(propertyInformation);

        Class[] groups = resolveGroups(facesContext, uiComponent);

        if (groups == null)
        {
            return null;
        }

        return BeanValidationUtils.validate(baseBeanClass, propertyName, convertedObject, groups, cascadedValidation);
    }

    Class getBaseClassType(PropertyInformation propertyInformation)
    {
        Class result = ExtValUtils.getPropertyDetails(propertyInformation).getBaseObject().getClass();

        return ProxyUtils.getUnproxiedClass(result);
    }

    String getPropertyToValidate(PropertyInformation propertyInformation)
    {
        return ExtValUtils.getPropertyDetails(propertyInformation).getProperty();
    }

    Class[] resolveGroups(FacesContext facesContext, UIComponent uiComponent)
    {
        return ExtValBeanValidationContext.getCurrentInstance().getGroups(
                facesContext.getViewRoot().getViewId(),
                uiComponent.getClientId(facesContext));
    }
}
