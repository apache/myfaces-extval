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

import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils;
import org.apache.myfaces.extensions.validator.beanval.util.BeanValidationUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.validation.ConstraintViolation;
import javax.validation.groups.Default;
import javax.validation.metadata.ElementDescriptor;
import java.util.Collections;
import java.util.Set;
import java.util.logging.Logger;

/**
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
class MappedConstraintSourceBeanValidationModuleValidationInterceptorInternals
{
    private Logger logger;
    private BeanValidationModuleValidationInterceptorInternals bviUtils;

    MappedConstraintSourceBeanValidationModuleValidationInterceptorInternals(
            Logger logger, BeanValidationModuleValidationInterceptorInternals bviUtils)
    {
        this.logger = logger;
        this.bviUtils = bviUtils;
    }

    void initComponentWithPropertyDetailsOfMappedConstraintSource(FacesContext facesContext,
                                                                  UIComponent uiComponent,
                                                                  PropertyDetails propertyDetails)
    {
        Class[] foundGroups = this.bviUtils.resolveGroups(facesContext, uiComponent);

        if (foundGroups == null)
        {
            return;
        }
        else if (foundGroups.length == 0)
        {
            foundGroups = new Class[]{Default.class};
        }

        PropertyDetails constraintSourcePropertyDetails = resolveMappedConstraintSourceFor(
                propertyDetails.getKey(), propertyDetails.getBaseObject().getClass(), propertyDetails.getProperty());

        if(constraintSourcePropertyDetails == null)
        {
            return;
        }

        ElementDescriptor elementDescriptor =
                BeanValidationUtils.getElementDescriptor((Class) constraintSourcePropertyDetails.getBaseObject(),
                        constraintSourcePropertyDetails.getProperty());

        if (elementDescriptor == null)
        {
            return;
        }

        this.bviUtils.processElementDescriptor(facesContext, uiComponent, foundGroups, elementDescriptor);
    }

    Set<ConstraintViolation<Object>> validateMappedConstraintSource(FacesContext facesContext,
                                                            UIComponent uiComponent,
                                                            Object convertedObject,
                                                            PropertyInformation propertyInformation,
                                                            boolean cascadedValidation)
    {
        Class baseBeanClass = this.bviUtils.getBaseClassType(propertyInformation);
        String propertyName = this.bviUtils.getPropertyToValidate(propertyInformation);
        String originalKey = getKey(propertyInformation);

        PropertyDetails constraintSourcePropertyDetails =
                resolveMappedConstraintSourceFor(originalKey, baseBeanClass, propertyName);

        if(constraintSourcePropertyDetails == null)
        {
            return Collections.emptySet();
        }

        baseBeanClass = (Class) constraintSourcePropertyDetails.getBaseObject();
        propertyName = constraintSourcePropertyDetails.getProperty();

        Class[] groups = this.bviUtils.resolveGroups(facesContext, uiComponent);

        if (groups == null)
        {
            return null;
        }

        return BeanValidationUtils.validate(baseBeanClass, propertyName, convertedObject, groups, cascadedValidation);
    }

    private String getKey(PropertyInformation propertyInformation)
    {
        return ExtValUtils.getPropertyDetails(propertyInformation).getKey();
    }

    PropertyDetails resolveMappedConstraintSourceFor(String originalKey, Class baseBeanClass, String property)
    {
        return ConstraintSourceUtils.resolveMappedConstraintSourceFor(originalKey, baseBeanClass, property);
    }
}
