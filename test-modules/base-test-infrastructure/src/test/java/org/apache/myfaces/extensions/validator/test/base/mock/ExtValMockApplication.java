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
package org.apache.myfaces.extensions.validator.test.base.mock;

import org.apache.myfaces.test.mock.MockApplication;

import javax.el.ELContextListener;
import javax.el.ELResolver;
import javax.el.ExpressionFactory;
import javax.el.ELException;
import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.context.FacesContext;
import java.util.ResourceBundle;

/**
 * @author Gerhard Petracek
 */
public class ExtValMockApplication extends MockApplication
{
    private Application wrapped;

    public ExtValMockApplication()
    {
        super();
    }

    public ExtValMockApplication(Application application)
    {
        super();
        this.wrapped = application;
    }

    @Override
    public ELContextListener[] getELContextListeners()
    {
        return new ELContextListener[0];
    }

    @Override
    public ELResolver getELResolver()
    {
        return this.wrapped.getELResolver();
    }

    @Override
    public ExpressionFactory getExpressionFactory()
    {
        return new ExtValMockExpressionFactory();
    }

    @Override
    public Object evaluateExpressionGet(FacesContext facesContext, String expression, Class aClass) throws ELException
    {
        return wrapped.evaluateExpressionGet(facesContext, expression, aClass);
    }

    @Override
    public ResourceBundle getResourceBundle(FacesContext ctx, String name) throws FacesException, NullPointerException
    {
        // TODO We can try here to load a resource bundle.
        return null;

    }
}