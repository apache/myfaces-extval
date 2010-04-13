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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.ValidationModuleAware;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.beanval.BeanValidationModuleKey;
import org.apache.myfaces.extensions.validator.beanval.util.BeanValidationUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.JsfUtils;

import javax.faces.component.UIComponent;
import java.util.Map;

/**
 * extracts and adds the extval bv meta-data (e.g. validation groups) to the ExtValBeanValidationContext
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@InvocationOrder(200)
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValBeanValidationMetaDataExtractionInterceptor
        implements MetaDataExtractionInterceptor, ValidationModuleAware
{
    public void afterExtracting(PropertyInformation propertyInformation)
    {
        if(propertyInformation.containsInformation(PropertyInformationKeys.CUSTOM_PROPERTIES))
        {
            Map properties = propertyInformation.getInformation(PropertyInformationKeys.CUSTOM_PROPERTIES, Map.class);

            if(properties != null && properties.containsKey(UIComponent.class.getName()))
            {
                UIComponent uiComponent = (UIComponent)properties.get(UIComponent.class.getName());
                PropertyDetails propertyDetails = ExtValUtils.getPropertyDetails(propertyInformation);

                if(propertyDetails != null)
                {
                    processExtValBeanValidationMetaData(uiComponent, propertyDetails);
                }
            }
        }
    }

    /**
     * adds the extval bv meta-data to the ExtValBeanValidationContext
     *
     * @param uiComponent current component
     * @param propertyDetails property details of the value-binding
     */
    private void processExtValBeanValidationMetaData(UIComponent uiComponent, PropertyDetails propertyDetails)
    {
        if(propertyDetails.getKey() == null)
        {
            return;
        }
        
        if(JsfUtils.isRenderResponsePhase())
        {
            BeanValidationUtils.addMetaDataToContext(uiComponent, propertyDetails, false);
        }
        else
        {
            BeanValidationUtils.addMetaDataToContext(uiComponent, propertyDetails, true);
        }
    }

    public String[] getModuleKeys()
    {
        return new String[] {BeanValidationModuleKey.class.getName()};
    }
}
