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
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.trinidad.validator.DoubleRangeValidator;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@ToDo(value = Priority.MEDIUM, description = "skipValidationSupport for client-side validation")
public class DoubleRangeInitializer extends TrinidadComponentInitializer
{
    @Override
    public boolean configureTrinidadComponent(FacesContext facesContext, UIComponent uiComponent,
                                              Map<String, Object> metaData)
    {
        boolean informationAdded = false;
        DoubleRangeValidator lengthValidator = (DoubleRangeValidator)facesContext.getApplication()
                                            .createValidator("org.apache.myfaces.trinidad.DoubleRange");

        if(metaData.containsKey(CommonMetaDataKeys.RANGE_MIN))
        {
            Object min = metaData.get(CommonMetaDataKeys.RANGE_MIN);

            if(min instanceof Double)
            {
                lengthValidator.setMinimum((Double)min);
                informationAdded = true;
            }
        }

        if(metaData.containsKey(CommonMetaDataKeys.RANGE_MAX))
        {
            Object maxLength = metaData.get(CommonMetaDataKeys.RANGE_MAX);

            if(maxLength instanceof Double)
            {
                lengthValidator.setMaximum((Double)maxLength);
                informationAdded = true;
            }
        }
        if(informationAdded)
        {
            ((EditableValueHolder)uiComponent).addValidator(lengthValidator);
            return true;
        }
        return false;
    }
}