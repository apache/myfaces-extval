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

import org.apache.myfaces.extensions.validator.core.initializer.component
        .AbstractHtmlCoreComponentsComponentInitializer;
import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@InvocationOrder(200)
@UsageInformation(UsageCategory.INTERNAL)
public class HtmlCoreComponentsComponentInitializer extends AbstractHtmlCoreComponentsComponentInitializer
{
    protected void configureRequiredAttribute(FacesContext facesContext,
                                              UIComponent uiComponent,
                                              Map<String, Object> metaData)
    {
        if(!ExtValUtils.interpretEmptyStringValuesAsNull())
        {
            return;
        }

        if(!((EditableValueHolder)uiComponent).isRequired() &&
                isRequiredInitializationRequested(metaData) &&
                isRequiredInitializationSupported(uiComponent))
        {
            ((EditableValueHolder)uiComponent).setRequired(true);
        }
    }

    private boolean isRequiredInitializationRequested(Map<String, Object> metaData)
    {
        return Boolean.TRUE.equals(metaData.get(CommonMetaDataKeys.REQUIRED)) ||
                Boolean.TRUE.equals(metaData.get(CommonMetaDataKeys.WEAK_REQUIRED));
    }
}