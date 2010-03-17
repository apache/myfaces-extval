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

import org.apache.myfaces.extensions.validator.core.interceptor.AbstractRendererInterceptor;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.render.Renderer;
import javax.faces.validator.Validator;
import java.io.IOException;

/**
 * separated for easier sync
 *
 * @author Gerhard Petracek
 * @since 2.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ResetBeanValidationRendererInterceptor extends AbstractRendererInterceptor
{
    @Override
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        if(uiComponent instanceof EditableValueHolder)
        {
            switchBackValidators((EditableValueHolder)uiComponent);
        }
    }

    private void switchBackValidators(EditableValueHolder editableValueHolder)
    {
        for(Validator validator : editableValueHolder.getValidators())
        {
            if(validator instanceof BeanValidatorWrapper)
            {
                editableValueHolder.addValidator(((BeanValidatorWrapper)validator).getWrappedBeanValidator());
                editableValueHolder.removeValidator(validator);
            }
        }
    }
}
