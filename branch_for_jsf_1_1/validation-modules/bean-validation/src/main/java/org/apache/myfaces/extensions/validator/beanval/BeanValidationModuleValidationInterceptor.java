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

import org.apache.myfaces.extensions.validator.core.interceptor.AbstractValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.beanval.util.BeanValidationUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.validation.ConstraintViolation;
import java.util.Set;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class BeanValidationModuleValidationInterceptor extends AbstractValidationInterceptor
{
    private BeanValidationModuleValidationInterceptorInternals bviUtils =
            new BeanValidationModuleValidationInterceptorInternals(this.logger);

    @Override
    protected boolean isRequiredInitializationSupported()
    {
        return true;
    }

    protected void initComponent(FacesContext facesContext, UIComponent uiComponent)
    {
        if (logger.isTraceEnabled())
        {
            logger.trace("start to init component " + uiComponent.getClass().getName());
        }

        PropertyDetails propertyDetails = bviUtils.extractPropertyDetails(
                facesContext, uiComponent, getPropertiesForComponentMetaDataExtractor(uiComponent));

        if (propertyDetails != null)
        {
            bviUtils.initComponentWithPropertyDetails(facesContext, uiComponent, propertyDetails);
        }

        if (logger.isTraceEnabled())
        {
            logger.trace("init component of " + uiComponent.getClass().getName() + " finished");
        }
    }

    protected void processValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject)
    {
        PropertyInformation propertyInformation = getPropertyInformation(facesContext, uiComponent);

        boolean validateProperty = hasBeanValidationConstraints(propertyInformation);
        try
        {
            if (validateProperty)
            {
                if (logger.isTraceEnabled())
                {
                    logger.trace("jsr303 start validation");
                }

                processFieldValidation(facesContext, uiComponent, convertedObject, propertyInformation);
            }
        }
        finally
        {
            if (validateProperty)
            {
                if (logger.isTraceEnabled())
                {
                    logger.trace("jsr303 validation finished");
                }
            }
        }
    }

    protected MetaDataExtractor getComponentMetaDataExtractor(Map<String, Object> properties)
    {
        return bviUtils.getComponentMetaDataExtractor(properties);
    }

    protected boolean hasBeanValidationConstraints(PropertyInformation propertyInformation)
    {
        return this.bviUtils.hasBeanValidationConstraints(propertyInformation);
    }

    protected void processFieldValidation(FacesContext facesContext,
                                          UIComponent uiComponent,
                                          Object convertedObject,
                                          PropertyInformation propertyInformation)
    {
        /*not used yet supportMultipleViolationsPerField()*/
        Set<ConstraintViolation> violations = this.bviUtils
                .validate(facesContext, uiComponent, convertedObject, propertyInformation);

        if(violations != null && !violations.isEmpty())
        {
            BeanValidationUtils.processConstraintViolations(facesContext, uiComponent, convertedObject, violations);
        }
    }

    @Override
    protected Class getModuleKey()
    {
        return BeanValidationModuleKey.class;
    }
}
