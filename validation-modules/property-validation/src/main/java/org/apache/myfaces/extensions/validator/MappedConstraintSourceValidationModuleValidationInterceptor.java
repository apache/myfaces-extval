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
package org.apache.myfaces.extensions.validator;

import java.util.Map;
import java.util.Collections;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValAnnotationUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import static org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils.resolveMappedConstraintSourceFor;

/**
 * @author Rudy De Busscher
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
public class MappedConstraintSourceValidationModuleValidationInterceptor
        extends PropertyValidationModuleValidationInterceptor
{
    @Override
    protected Map<String, Object> getTransformedMetaDataFor(FacesContext facesContext, UIComponent uiComponent)
    {
        PropertyDetails originalPropertyDetail = ExtValUtils.getELHelper()
                .getPropertyDetailsOfValueBinding(uiComponent);

        PropertyInformation propertyInformation = extractFromMappedConstraintSource(originalPropertyDetail);

        if (propertyInformation == null)
        {
            // No @ConstraintSource or alike so nothing to do.
            return Collections.emptyMap();
        }

        return ExtValUtils.getTransformedMetaDataFor(facesContext, propertyInformation, getModuleKey());
    }

    @Override
    protected void processFieldValidation(FacesContext facesContext,
                                          UIComponent uiComponent,
                                          Object convertedObject,
                                          PropertyInformation propertyInformation)
    {
        PropertyDetails originalPropertyDetail = ExtValUtils.getPropertyDetails(propertyInformation);

        propertyInformation = extractFromMappedConstraintSource(originalPropertyDetail);

        if (propertyInformation != null)
        {
            super.processFieldValidation(facesContext, uiComponent, convertedObject, propertyInformation);
        }
    }

    private PropertyInformation extractFromMappedConstraintSource(PropertyDetails originalPropertyDetail)
    {
        PropertyDetails constraintSourcePropertyDetails =
                resolveMappedConstraintSourceFor(originalPropertyDetail.getKey(),
                                                 originalPropertyDetail.getBaseObject().getClass(),
                                                 originalPropertyDetail.getProperty());

        if (constraintSourcePropertyDetails == null)
        {
            return null;
        }

        return ExtValAnnotationUtils.extractAnnotations(
                (Class) constraintSourcePropertyDetails.getBaseObject(), constraintSourcePropertyDetails);
    }

    @Override
    protected boolean recordProcessedInformation()
    {
        // Nothing to do here, has already been done.
        return false;
    }
}
