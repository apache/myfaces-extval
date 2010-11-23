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
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.validation.ConstraintViolation;
import java.util.Set;

/**
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
public class MappedConstraintSourceBeanValidationModuleValidationInterceptor
        extends BeanValidationModuleValidationInterceptor
{
    MappedConstraintSourceBeanValidationModuleValidationInterceptorInternals csaBviUtils =
            new MappedConstraintSourceBeanValidationModuleValidationInterceptorInternals(this.logger, this.bviUtils);

    @Override
    protected void initComponentWithPropertyDetails(FacesContext facesContext,
                                                    UIComponent uiComponent,
                                                    PropertyDetails propertyDetails)
    {
        this.csaBviUtils.initComponentWithPropertyDetailsOfMappedConstraintSource(
                facesContext, uiComponent, propertyDetails);
    }

    @Override
    protected boolean hasBeanValidationConstraints(PropertyInformation propertyInformation)
    {
        PropertyDetails propertyDetails = propertyInformation.getInformation(
                PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        return this.csaBviUtils.resolveMappedConstraintSourceFor(propertyDetails.getKey(),
                                                              propertyDetails.getBaseObject().getClass(),
                                                              propertyDetails.getProperty()) != null;
    }

    @Override
    protected void processFieldValidation(FacesContext facesContext,
                                          UIComponent uiComponent,
                                          Object convertedObject,
                                          PropertyInformation propertyInformation)
    {
        Set<ConstraintViolation<Object>> violations = this.csaBviUtils
                .validateMappedConstraintSource(facesContext, uiComponent, convertedObject, propertyInformation, true);

        processConstraintViolations(facesContext, uiComponent, convertedObject, violations);
    }
}
