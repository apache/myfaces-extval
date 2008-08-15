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
package org.apache.myfaces.extensions.validator.core.adapter;

import org.apache.myfaces.extensions.validator.core.ExtValConverter;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

/**
 * just for the fallback solution
 *
 * @author Gerhard Petracek
 */
@Deprecated
public class ExtValFallbackConverter extends ExtValConverter
{
    private Converter wrapped;

    public ExtValFallbackConverter(Converter converter)
    {
        this.wrapped = converter;
    }

    @Override
    public String getAsString(FacesContext facesContext,
            UIComponent uiComponent, Object o)
    {
        if (this.wrapped == null)
        {
            //indirect approach for complex components
            Converter converter = ExtValUtils.tryToCreateOriginalConverter(
                    facesContext, uiComponent);
            if (converter == null)
            {
                return (o == null) ? null : o.toString();
            }
            else
            {
                return converter.getAsString(facesContext, uiComponent, o);
            }
        }
        else
        {
            return this.wrapped.getAsString(facesContext, uiComponent, o);
        }
    }

    @Override
    protected Object getConvertedObject(FacesContext facesContext,
            UIComponent uiComponent, String s)
    {
        if (this.wrapped == null)
        {
            //indirect approach for complex components
            //TODO
            Converter converter = ExtValUtils.tryToCreateOriginalConverter(
                    facesContext, uiComponent);

            return (converter != null) ? converter.getAsObject(facesContext,
                    uiComponent, s) : s;
        }
        else
        {
            return this.wrapped.getAsObject(facesContext, uiComponent, s);
        }
    }
}
