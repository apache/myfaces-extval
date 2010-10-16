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
package org.apache.myfaces.extensions.validator.core.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameter;
import org.apache.myfaces.extensions.validator.core.InvocationOrderSupport;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import java.util.Map;

/**
 * Interface to define interceptors that can perform logic before and after the validations are executed.
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@InvocationOrderSupport
@UsageInformation(UsageCategory.API)
public interface PropertyValidationInterceptor extends ValidationParameter
{
    /**
     * Executed before the RendererInterceptor calls the validation Strategies to validate the converted value.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The component which is processed
     * @param convertedObject  The converted object
     * @param properties Additional information of interest. Contains the PropertyInformation object.
     * @return false if the validation process should be bypassed
     */
    boolean beforeValidation(FacesContext facesContext,
                             UIComponent uiComponent,
                             Object convertedObject,
                             Map<String, Object> properties);

    /**
     * Processed if validation was executed
     * in contrast to ValidationExceptionInterceptor it gets executed in any case.
     * @param facesContext The JSF Context
     * @param uiComponent The component which is processed
     * @param convertedObject The converted object
     * @param properties Additional information of interest. Contains the PropertyInformation object.
     */
    void afterValidation(FacesContext facesContext,
                         UIComponent uiComponent,
                         Object convertedObject,
                         Map<String, Object> properties);
}
