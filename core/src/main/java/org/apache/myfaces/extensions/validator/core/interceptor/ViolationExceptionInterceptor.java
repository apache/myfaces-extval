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
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;

import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.validator.ValidatorException;
import javax.faces.context.FacesContext;

/**
 * This validation-exception interceptor sets the valid property of the uiComponent to false,
 * if it is a blocking severity.
 * 
 * @since x.x.3
 */
@InvocationOrder(200)
@UsageInformation(UsageCategory.INTERNAL)
public class ViolationExceptionInterceptor implements ValidationExceptionInterceptor
{

    public boolean afterThrowing(UIComponent uiComponent,
                                 MetaDataEntry metaDataEntry,
                                 Object convertedObject,
                                 ValidatorException validatorException,
                                 ValidationStrategy validatorExceptionSource)
    {
        if(uiComponent instanceof EditableValueHolder && isBlockingException(uiComponent, validatorException))
        {
            //bv integration doesn't throw exceptions to support multiple messages -> set component state
            ((EditableValueHolder)uiComponent).setValid(false);
        }
        return true;
    }

    private boolean isBlockingException(UIComponent uiComponent, ValidatorException validatorException)
    {
        return ExtValContext.getContext().getViolationSeverityInterpreter().severityCausesValidatorException(
                FacesContext.getCurrentInstance(),
                uiComponent,
                validatorException.getFacesMessage().getSeverity());
    }
}