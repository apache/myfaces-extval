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
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.beanval.ExtValBeanValidationContext;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.validator.BeanValidator;
import javax.faces.validator.Validator;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @since 2.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class BeanValidationTagAwareValidationInterceptor implements PropertyValidationInterceptor
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    public boolean beforeValidation(FacesContext facesContext,
                             UIComponent uiComponent,
                             Object convertedObject,
                             Map<String, Object> properties)
    {
        if(uiComponent instanceof EditableValueHolder && //"filter" cross-validation calls
                properties.containsKey(PropertyInformation.class.getName()))
        {
            inspectValidators(facesContext.getViewRoot().getViewId(),
                    (EditableValueHolder)uiComponent,
                    uiComponent.getClientId(facesContext),
                    ((EditableValueHolder)uiComponent).getValidators());
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

    @ToDo.List({@ToDo(value = Priority.HIGH, description = "optimize"),
            @ToDo(value = Priority.HIGH, description = "use reflection instead of BeanValidator for jsf 1.x versions"),
            @ToDo(value = Priority.HIGH, description = "test")
    })
    private void inspectValidators(String viewId,
                                   EditableValueHolder editableValueHolder,
                                   String clientId,
                                   Validator[] validators)
    {
        List<String> groupsClassNamesOfTagList = new ArrayList<String>();

        for (Validator validator : validators)
        {
            if(validator instanceof BeanValidator)
            {
                if(((BeanValidator) validator).getValidationGroups() != null)
                {
                    groupsClassNamesOfTagList.addAll(
                            Arrays.asList(((BeanValidator) validator).getValidationGroups().split(",")));
                }

                if (validator.getClass().getName().equals(BeanValidator.class.getName()))
                {
                    //prevent double-validation
                    editableValueHolder.removeValidator(validator);
                    editableValueHolder.addValidator(
                            new ExtValBeanValidator(((BeanValidator)validator).getValidationGroups()));
                }
            }
        }

        ExtValBeanValidationContext beanValidationContext = ExtValBeanValidationContext.getCurrentInstance();
        Class currentClass;
        for(String groupClassName : groupsClassNamesOfTagList)
        {
            currentClass = ClassUtils.tryToLoadClassForName(groupClassName.trim());

            if(currentClass != null && currentClass.isInterface())
            {
                beanValidationContext.addGroup(currentClass, viewId, clientId);
            }
            else
            {
                this.logger.severe(groupClassName + " is no valid group - only existing interfaces are allowed");
            }
        }
    }
}