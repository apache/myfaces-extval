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
package org.apache.myfaces.extensions.validator.trinidad.storage;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.trinidad.ExtValTrinidadClientValidatorWrapper;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.validator.Validator;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultClientValidatorStorage implements TrinidadClientValidatorStorage
{
    private List<UIComponent> componentList = new ArrayList<UIComponent>();

    public void addComponent(UIComponent trinidadComponent)
    {
        if(!this.componentList.contains(trinidadComponent))
        {
            this.componentList.add(trinidadComponent);
        }
    }

    public void rollback()
    {
        for (UIComponent component : this.componentList)
        {
            removeTrinidadValidatorWrapper(component);
        }
        this.componentList.clear();
    }

    private void removeTrinidadValidatorWrapper(UIComponent uiComponent)
    {
        if (uiComponent instanceof EditableValueHolder)
        {
            removeWrapperFromComponent(uiComponent);
        }
        else
        {
            //to keep the source in sync with older versions
            for (Object child : uiComponent.getChildren())
            {
                removeTrinidadValidatorWrapper((UIComponent)child);
            }
        }
    }

    private void removeWrapperFromComponent(UIComponent uiComponent)
    {
        for (Validator validator : ((EditableValueHolder) uiComponent).getValidators())
        {
            if (validator instanceof ExtValTrinidadClientValidatorWrapper)
            {
                ((EditableValueHolder) uiComponent).removeValidator(validator);
            }
        }
    }
}
