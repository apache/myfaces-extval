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

import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.storage.FacesMessageStorage;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @since x.x.3
 */
@InvocationOrder(900)
@UsageInformation(UsageCategory.INTERNAL)
public class FacesMessagePropertyValidationInterceptor implements PropertyValidationInterceptor
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    public boolean beforeValidation(
            FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
    {
        return true;
    }

    public void afterValidation(
            FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
    {
        FacesMessageStorage facesMessageStorage = ExtValUtils.getStorage(
                FacesMessageStorage.class, FacesMessageStorage.class.getName());

        if(facesMessageStorage != null)
        {
            facesMessageStorage.addAll();
            ExtValUtils.resetStorage(FacesMessageStorage.class, FacesMessageStorage.class.getName());
        }
    }
}
