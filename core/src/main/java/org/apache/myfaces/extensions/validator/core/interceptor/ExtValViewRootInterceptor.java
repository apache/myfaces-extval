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

import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.factory.ExtValAjaxBehavior;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.validation.SkipConstraintValidation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import javax.el.MethodExpression;
import javax.faces.component.ActionSource2;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.behavior.AjaxBehavior;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.AjaxBehaviorEvent;
import javax.faces.event.FacesEvent;
import java.lang.reflect.Method;
import java.util.List;

public class ExtValViewRootInterceptor implements ViewRootInterceptor
{
    public void afterQueueEvent(FacesEvent event)
    {
        UIComponent uiComponent = event.getComponent();
        if(event instanceof ActionEvent && uiComponent instanceof ActionSource2)
        {
            tryToProcessActionMethod((ActionSource2)event.getComponent());
        }
        if (event instanceof AjaxBehaviorEvent && uiComponent instanceof EditableValueHolder &&
                ((AjaxBehaviorEvent)event).getBehavior() instanceof AjaxBehavior)
        {
            tryToProcessAjaxListener(((AjaxBehavior)((AjaxBehaviorEvent)event).getBehavior()));
        }
    }

    protected void tryToProcessActionMethod(ActionSource2 commandComponent)
    {
        MethodExpression actionExpression = commandComponent.getActionExpression();

        if(actionExpression == null)
        {
            return;
        }

        ELHelper elHelper = ExtValUtils.getELHelper();
        String actionString = actionExpression.getExpressionString();
        if(!elHelper.isELTermWellFormed(actionString))
        {
            return;
        }

        ValueBindingExpression valueBindingExpression = new ValueBindingExpression(actionString);

        FacesContext facesContext = FacesContext.getCurrentInstance();
        if (!ExtValUtils.getELHelper().isELTermValid(
                facesContext, valueBindingExpression.getBaseExpression().getExpressionString()))
        {
            return;
        }

        processBypassValidation(facesContext, valueBindingExpression, elHelper, ActionEvent.class);
    }

    protected void tryToProcessAjaxListener(AjaxBehavior ajaxBehavior)
    {
        if (!(ajaxBehavior instanceof ExtValAjaxBehavior))
        {
            return;
        }

        List<MethodExpression> listenerExpressions = ((ExtValAjaxBehavior)ajaxBehavior).getListenerExpressions();

        ELHelper elHelper = ExtValUtils.getELHelper();

        for (MethodExpression listenerExpression : listenerExpressions)
        {
            String actionString = listenerExpression.getExpressionString();
            if(!elHelper.isELTermWellFormed(actionString))
            {
                continue;
            }

            ValueBindingExpression valueBindingExpression = new ValueBindingExpression(actionString);

            FacesContext facesContext = FacesContext.getCurrentInstance();
            if (!ExtValUtils.getELHelper().isELTermValid(
                    facesContext, valueBindingExpression.getBaseExpression().getExpressionString()))
            {
                continue;
            }

            processBypassValidation(facesContext, valueBindingExpression, elHelper, AjaxBehaviorEvent.class);
        }
    }

    protected void processBypassValidation(FacesContext facesContext,
                                           ValueBindingExpression valueBindingExpression,
                                           ELHelper elHelper,
                                           Class... parameterTypes)
    {
        Object base = elHelper.getValueOfExpression(facesContext, valueBindingExpression.getBaseExpression());

        if (base == null)
        {
            return;
        }

        String methodName = valueBindingExpression.getProperty();

        if (methodName.contains("("))
        {
            //currently we just can support action-listeners and action-methods without parameters
            methodName = methodName.substring(0, methodName.indexOf("("));
        }

        Method actionMethod = ReflectionUtils.tryToGetMethod(ProxyUtils.getUnproxiedClass(base.getClass()), methodName);

        if (actionMethod == null)
        {
            actionMethod = ReflectionUtils.tryToGetMethod(ProxyUtils.getUnproxiedClass(base.getClass()),
                    methodName, parameterTypes);
        }

        if (actionMethod == null)
        {
            //TODO log unsupported expression
            return;
        }

        SkipConstraintValidation skipConstraintValidation =
                actionMethod.getAnnotation(SkipConstraintValidation.class);

        if(skipConstraintValidation == null)
        {
            return;
        }

        facesContext.getExternalContext().getRequestMap().put(PropertyInformationKeys.SKIP_VALIDATION, Boolean.TRUE);
    }
}
