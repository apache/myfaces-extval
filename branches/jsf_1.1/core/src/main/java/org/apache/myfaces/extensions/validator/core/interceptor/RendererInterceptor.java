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

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface RendererInterceptor
{
    String getInterceptorId();
    /*
     * before
     */
    void beforeDecode(FacesContext facesContext, UIComponent uiComponent, Renderer renderer);

    void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException;

    void beforeEncodeChildren(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException;

    void beforeEncodeEnd(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException;

    void beforeConvertClientId(FacesContext facesContext, String s, Renderer renderer);

    void beforeGetRendersChildren(Renderer renderer);

    void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer renderer)
        throws ConverterException;

    /*
     * before delegation
     */

    /*
     * after
     */
    void afterDecode(FacesContext facesContext, UIComponent uiComponent, Renderer renderer);

    void afterEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException;

    void afterEncodeChildren(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException;

    void afterEncodeEnd(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException;

    void afterConvertClientId(FacesContext facesContext, String s, Renderer renderer);

    void afterGetRendersChildren(Renderer renderer);

    void afterGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer renderer)
        throws ConverterException;
}