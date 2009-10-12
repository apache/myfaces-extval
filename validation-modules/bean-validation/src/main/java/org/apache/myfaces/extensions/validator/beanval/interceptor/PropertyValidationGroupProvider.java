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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.beanval.BeanValidationModuleKey;
import org.apache.myfaces.extensions.validator.core.ValidationModuleAware;
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;

/**
 * validation interceptor which extracts all groups for the validation process
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@Deprecated
public class PropertyValidationGroupProvider implements PropertyValidationInterceptor, ValidationModuleAware
{
    protected final Log logger = LogFactory.getLog(getClass());

    public boolean beforeValidation(FacesContext facesContext,
                                    UIComponent uiComponent,
                                    Object convertedObject,
                                    Map<String, Object> properties)
    {
        return true;
    }

    public void afterValidation(FacesContext facesContext,
                                UIComponent uiComponent,
                                Object convertedObject,
                                Map<String, Object> properties)
    {
        //not used
    }

    public String[] getModuleKeys()
    {
        return new String[]{BeanValidationModuleKey.class.getName()};
    }
}
