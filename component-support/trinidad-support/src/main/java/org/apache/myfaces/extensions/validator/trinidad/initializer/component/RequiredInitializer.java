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
package org.apache.myfaces.extensions.validator.trinidad.initializer.component;

import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class RequiredInitializer extends TrinidadComponentInitializer
{
    @Override
    public boolean configureTrinidadComponent(FacesContext facesContext, UIComponent uiComponent,
                                              Map<String, Object> metaData)
    {
        if(metaData.containsKey(CommonMetaDataKeys.REQUIRED) ||
           metaData.containsKey(CommonMetaDataKeys.WEAK_REQUIRED)||
           metaData.containsKey(CommonMetaDataKeys.SKIP_VALIDATION))
        {
            if((Boolean.TRUE.equals(metaData.get(CommonMetaDataKeys.WEAK_REQUIRED)) ||
                 Boolean.TRUE.equals(metaData.get(CommonMetaDataKeys.REQUIRED)))
                &&
                Boolean.TRUE.equals(isComponentRequired(uiComponent)))
            {
                ((EditableValueHolder)uiComponent).setRequired(true);
                return true;
            }
            else if(Boolean.TRUE.equals(metaData.get(CommonMetaDataKeys.SKIP_VALIDATION)) &&
                   !Boolean.TRUE.equals(metaData.get(CommonMetaDataKeys.REQUIRED)))
            {
                ((EditableValueHolder)uiComponent).setRequired(false);
                return true;
            }
        }
        return false;
    }

    protected Boolean isComponentRequired(UIComponent uiComponent)
    {
        //compare with false so true = true or null
        boolean isReadOnly = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isReadOnly")));
        boolean isDisabled = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isDisabled")));

        return !(isReadOnly || isDisabled);
    }
}