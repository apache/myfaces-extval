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
package org.apache.myfaces.trinidadinternal.convert;

import org.apache.myfaces.extensions.validator.core.adapter.ExtValFallbackConverter;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

/*generated code - don't change!*/
public class DoubleConverterAdapter extends
        org.apache.myfaces.trinidadinternal.convert.DoubleConverter
{
    private Converter smartConverter;

    public DoubleConverterAdapter()
    {
        this(new org.apache.myfaces.trinidadinternal.convert.DoubleConverter());
    }

    public DoubleConverterAdapter(Converter converter)
    {
        this.smartConverter = new ExtValFallbackConverter(converter);
    }

    public Object getAsObject(FacesContext context, UIComponent component,
            String value) throws ConverterException
    {
        return this.smartConverter.getAsObject(context, component, value);
    }

    public String getAsString(FacesContext context, UIComponent component,
            Object value) throws ConverterException
    {
        return this.smartConverter.getAsString(context, component, value);
    }
}
