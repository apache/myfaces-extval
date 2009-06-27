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
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.trinidad.ExtValTrinidadClientValidatorWrapper;
import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputLabel;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.LengthValidator;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@ToDo(value = Priority.MEDIUM, description = "skipValidationSupport for client-side validation")
@UsageInformation(UsageCategory.INTERNAL)
class LengthInitializer extends TrinidadComponentInitializer
{
    @Override
    public boolean configureTrinidadComponent(FacesContext facesContext, UIComponent uiComponent,
                                              Map<String, Object> metaData)
    {
        boolean informationAdded = false;
        //TODO
        LengthValidator lengthValidator = (LengthValidator)facesContext.getApplication()
                                            .createValidator("org.apache.myfaces.trinidad.Length");

        Object minLength = null;
        if(metaData.containsKey(CommonMetaDataKeys.MIN_LENGTH))
        {
            minLength = metaData.get(CommonMetaDataKeys.MIN_LENGTH);
        }
        else if(metaData.containsKey(CommonMetaDataKeys.MIN_LENGTH_DEFAULT))
        {
            minLength = metaData.get(CommonMetaDataKeys.MIN_LENGTH_DEFAULT);
        }

        if(minLength instanceof Integer)
        {
            lengthValidator.setMinimum((Integer)minLength);
            informationAdded = true;
        }

        Object maxLength = null;
        if(metaData.containsKey(CommonMetaDataKeys.MAX_LENGTH))
        {
            maxLength = metaData.get(CommonMetaDataKeys.MAX_LENGTH);
        }
        else if(metaData.containsKey(CommonMetaDataKeys.MAX_LENGTH_DEFAULT))
        {
            maxLength = metaData.get(CommonMetaDataKeys.MAX_LENGTH_DEFAULT);
        }

        if(maxLength instanceof Integer)
        {
            if(processComponent(uiComponent))
            {
                ReflectionUtils.tryToInvokeMethod(
                        uiComponent,
                        ReflectionUtils.tryToGetMethod(
                                uiComponent.getClass(),
                                "setMaximumLength",
                                int.class),
                        maxLength);
            }
            
            lengthValidator.setMaximum((Integer)maxLength);
            informationAdded = true;
        }

        //reInitValidators((EditableValueHolder)uiComponent, metaData); //search wrappers and call .deactivate

        if(informationAdded && lengthValidator instanceof ClientValidator)
        {
            if(uiComponent instanceof EditableValueHolder)
            {
                ((EditableValueHolder)uiComponent).addValidator(
                        new ExtValTrinidadClientValidatorWrapper((ClientValidator)lengthValidator));
            }
            else if (uiComponent instanceof CoreOutputLabel)
            {
                if(lengthValidator.getMinimum() > 0)
                {
                    ((CoreOutputLabel)uiComponent).setShowRequired(true);
                }

                if(Boolean.TRUE.equals(metaData.get(CommonMetaDataKeys.SKIP_VALIDATION)))
                {
                    ((CoreOutputLabel)uiComponent).setShowRequired(false);
                }
            }

            return true;
        }
        return false;
    }
}
